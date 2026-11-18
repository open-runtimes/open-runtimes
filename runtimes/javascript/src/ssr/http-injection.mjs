// Shared HTTP server wrappers used by both the node and bun SSR injection
// entrypoints. Contains the per-request logic (Open Runtimes endpoints, secret
// auth, per-request log id, safe execution timeout, error capture) plus the
// constructor wrappers that install it. Runtime entrypoints apply these via a
// direct Server.prototype.emit patch at preload time (node: --import, bun:
// --preload, deno: --import).

import {
  addOprEndpoints,
  addAuthenticationCheck,
  addSafeTimeout,
} from "./system.mjs";
import { Logger, loggingNamespace } from "./logger.mjs";

// A handler that keeps running after its execution timed out can still try to
// respond. The timeout already sent the response, so Node throws (for example
// ERR_HTTP_HEADERS_SENT), from a place the user's code cannot catch, and the
// server would stop for every other request. Report the error in the timed-out
// execution's logs instead. Any other uncaught error behaves as it does without
// the runtime.
function reportLateError(error) {
  const execution = loggingNamespace.getStore();
  if (!execution?.timedOut) {
    return false;
  }

  Logger.write(
    execution.id,
    ["Error after the execution timed out:", error],
    Logger.TYPE_ERROR,
  );
  return true;
}

process.on("uncaughtException", (error) => {
  if (reportLateError(error)) {
    return;
  }

  // With no listener of the app's own, keep Node's default: print and exit.
  if (process.listenerCount("uncaughtException") === 1) {
    process.stderr.write(`${error?.stack ?? error}\n`);
    process.exit(1);
  }
});

process.on("unhandledRejection", (reason) => {
  if (reportLateError(reason)) {
    return;
  }

  // Node's default for an unhandled rejection is to raise it as an uncaught exception.
  if (process.listenerCount("unhandledRejection") === 1) {
    throw reason;
  }
});

// Core: replace an http.Server's `emit` so "request" events flow through the
// Open Runtimes contract before hitting the user-owned listener.
export function overrideEmit(originalEmit) {
  return function (event, ...emitArgs) {
    if (event !== "request") {
      return originalEmit.call(this, event, ...emitArgs);
    }

    const [req, res] = emitArgs;

    if (addOprEndpoints(req, res)) {
      return;
    }

    if (addAuthenticationCheck(req, res)) {
      return;
    }

    req.loggerId = Logger.start(
      req.headers["x-open-runtimes-logging"],
      req.headers["x-open-runtimes-log-id"],
    );
    res.setHeader("x-open-runtimes-log-id", req.loggerId);

    if (addSafeTimeout(req, res)) {
      return;
    }

    const self = this;

    const execution = { id: req.loggerId, timedOut: false };

    loggingNamespace.run(execution, () => {
      Logger.overrideNativeLogs(loggingNamespace, req.loggerId);

      const dispatch = () => {
        try {
          originalEmit.call(self, event, ...emitArgs);
        } catch (error) {
          if (!res.headersSent) {
            Logger.write(req.loggerId, [error], Logger.TYPE_ERROR);
            res.writeHead(500, { "Content-Type": "text/plain" });
            res.end("");
          }
        }
      };

      if (req.safeTimeout !== null && req.safeTimeout !== undefined) {
        setTimeout(() => {
          if (!res.headersSent) {
            console.error("Execution timed out.");
            res.statusCode = 500;
            res.setHeader("Content-Type", "text/plain");
            res.setHeader("Content-Length", 0);
            res.end("");
            execution.timedOut = true;
          }
        }, req.safeTimeout * 1000);
      }

      dispatch();
    });
  };
}
