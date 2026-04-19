// Bun --preload entrypoint that injects the Open Runtimes HTTP contract
// (health + timings endpoints, secret auth, log-id header, safe execution
// timeout) into any user-supplied framework server that uses node's http
// module. Bun's http compat exposes Server.prototype.emit — wrapping it here
// gives the same coverage node gets through import-in-the-middle /
// require-in-the-middle in runtimes/node/versions/latest/src/ssr/injections.mjs,
// without needing either hook library.

import http from "node:http";
import https from "node:https";
import {
  addOprEndpoints,
  addAuthenticationCheck,
  addSafeTimeout,
} from "./system";
import { Logger, loggingNamespace } from "./logger";

console.log("Preparing SSR runtime ...");

function overrideEmit(originalEmit: Function) {
  return function (this: any, event: string, ...emitArgs: any[]) {
    if (event !== "request") {
      return originalEmit.call(this, event, ...emitArgs);
    }

    const [req, res] = emitArgs;

    // Internal health check, telemetry, etc.
    if (addOprEndpoints(req, res)) {
      return;
    }

    // Ensure auth headers
    if (addAuthenticationCheck(req, res)) {
      return;
    }

    // Enable per-request logging
    const loggerId = Logger.start(
      req.headers["x-open-runtimes-logging"],
      req.headers["x-open-runtimes-log-id"],
    );
    req.loggerId = loggerId;
    res.setHeader("x-open-runtimes-log-id", loggerId);

    // Validate and prepare safe timeout duration
    if (addSafeTimeout(req, res)) {
      return;
    }

    const self = this;

    loggingNamespace.run({ id: loggerId }, () => {
      Logger.overrideNativeLogs(loggingNamespace, loggerId);

      const dispatch = () => {
        try {
          originalEmit.call(self, event, ...emitArgs);
        } catch (error) {
          if (!res.headersSent) {
            Logger.write(loggerId, [error], Logger.TYPE_ERROR);
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

function wrapServerClass(originalServer: any) {
  if (!originalServer || originalServer.__oprWrapped) {
    return originalServer;
  }
  const wrapped: any = function (this: any, ...args: any[]) {
    const server = new originalServer(...args);
    if (!server.__oprEmitWrapped) {
      server.emit = overrideEmit(server.emit.bind(server));
      server.__oprEmitWrapped = true;
    }
    return server;
  };
  wrapped.prototype = originalServer.prototype;
  wrapped.__oprWrapped = true;
  return wrapped;
}

function wrapCreateServer(originalCreateServer: any) {
  if (!originalCreateServer || originalCreateServer.__oprWrapped) {
    return originalCreateServer;
  }
  const wrapped: any = function (...args: any[]) {
    const server = originalCreateServer(...args);
    if (!server.__oprEmitWrapped) {
      server.emit = overrideEmit(server.emit.bind(server));
      server.__oprEmitWrapped = true;
    }
    return server;
  };
  wrapped.__oprWrapped = true;
  return wrapped;
}

// Patch Server.prototype.emit directly so instances created via `new Server()`
// and `createServer()` both flow through the wrapped emit.
for (const mod of [http, https] as any[]) {
  if (!mod) continue;
  if (mod.Server && mod.Server.prototype && !mod.Server.prototype.__oprEmitPatched) {
    const originalEmit = mod.Server.prototype.emit;
    mod.Server.prototype.emit = overrideEmit(originalEmit);
    mod.Server.prototype.__oprEmitPatched = true;
  }
  if (mod.createServer) {
    mod.createServer = wrapCreateServer(mod.createServer);
  }
  if (mod.Server) {
    mod.Server = wrapServerClass(mod.Server);
  }
}

console.log(
  `SSR runtime prepared with configuration PORT=${process.env.PORT} and HOST=${process.env.HOST}`,
);
