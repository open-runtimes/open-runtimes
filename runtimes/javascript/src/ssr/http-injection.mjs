// Shared HTTP server wrappers used by both the node and bun SSR injection
// entrypoints. Contains the per-request logic (Open Runtimes endpoints, secret
// auth, per-request log id, safe execution timeout, error capture) plus the
// constructor wrappers that install it. Runtime entrypoints decide *how* these
// get applied (node: import-in-the-middle + require-in-the-middle; bun: direct
// Server.prototype.emit patch at preload time).

import {
  addOprEndpoints,
  addAuthenticationCheck,
  addSafeTimeout,
} from "./system.mjs";
import { Logger, loggingNamespace } from "./logger.mjs";

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

    loggingNamespace.run({ id: req.loggerId }, () => {
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
          }
        }, req.safeTimeout * 1000);
      }

      dispatch();
    });
  };
}

// Wrap a Server constructor (used by Nitro's `new Server()` path).
export function wrapServer(originalServer) {
  return function (...args) {
    const server = originalServer.apply(this, args);
    server.emit = overrideEmit(server.emit);
    return server;
  };
}

// Wrap createServer (used by native http, express, koa, ...).
export function wrapCreateServer(originalCreateServer) {
  return function (...args) {
    const server = new originalCreateServer(...args);
    server.emit = overrideEmit(server.emit);
    return server;
  };
}

// Apply wrapServer/wrapCreateServer to a loaded http/https module's exports.
// Handles both CJS and ESM shapes. Used by node's import-in-the-middle hook.
export function wrapHttp(moduleExports) {
  if (moduleExports.Server) {
    moduleExports.Server = wrapServer(moduleExports.Server);
  }
  if (moduleExports.createServer) {
    moduleExports.createServer = wrapCreateServer(moduleExports.createServer);
  }

  const isESM = moduleExports[Symbol.toStringTag] === "Module";
  if (isESM) {
    if (moduleExports.default.Server) {
      moduleExports.default.Server = wrapServer(moduleExports.default.Server);
    }
    if (moduleExports.default.createServer) {
      moduleExports.default.createServer = wrapCreateServer(
        moduleExports.default.createServer,
      );
    }
  }

  return moduleExports;
}
