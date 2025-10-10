import { Hook, createAddHookMessageChannel } from "import-in-the-middle";
import ritm from "require-in-the-middle";
import { register } from "node:module";
import {
  addOprEndpoints,
  addAuthenticationCheck,
  addSafeTimeout,
} from "./system.mjs";
import { Logger, loggingNamespace } from "./logger.mjs";

console.log(`Preparing SSR runtime ...`);

const { registerOptions, waitForAllMessagesAcknowledged } =
  createAddHookMessageChannel();
register("import-in-the-middle/hook.mjs", import.meta.url, registerOptions);

// Main logic to override HTTP and HTTPS libraries
function wrapHttp(exports) {
  if (exports.Server) {
    exports.Server = wrapServer(exports.Server);
  }
  if (exports.createServer) {
    exports.createServer = wrapCreateServer(exports.createServer);
  }
  return exports;
}

// Override for methods used by Nitro
function wrapServer(originalServer) {
  return function (...args) {
    const server = originalServer.apply(this, args);
    server.emit = overrideEmit(server.emit);
    return server;
  };
}

// Override for methods used by native http, express.js, koa.js, ...
function wrapCreateServer(originalCreateServer) {
  return function (...args) {
    const server = new originalCreateServer(...args);
    server.emit = overrideEmit(server.emit);
    return server;
  };
}

// Request handler overrides (code here is main purpose of injection)
function overrideEmit(originalEmit) {
  return function (event, ...emitArgs) {
    if (event !== "request") {
      return originalEmit.call(this, event, ...emitArgs);
    }

    const [req, res] = emitArgs;
    let handled = false;

    // internal health check, telemetry, etc.
    handled = addOprEndpoints(req, res);
    if (handled) {
      return;
    }

    // Ensure auth headers
    handled = addAuthenticationCheck(req, res);
    if (handled) {
      return;
    }

    // Enable per-request logging
    req.loggerId = Logger.start(
      req.headers[`x-open-runtimes-logging`],
      req.headers[`x-open-runtimes-log-id`],
    );
    res.setHeader("x-open-runtimes-log-id", req.loggerId);

    // Validate and prepare safe timeout duration
    handled = addSafeTimeout(req, res);
    if (handled) {
      return;
    }

    // Wrap rest of the logic in namespace for proper log reporting
    loggingNamespace.run(async () => {
      loggingNamespace.set("id", req.loggerId);
      Logger.overrideNativeLogs(loggingNamespace, req.loggerId);

      // Apply safe timeout, when relevant
      if (req.safeTimeout !== null) {
        setTimeout(() => {
          if (!res.headersSent) {
            console.error("Execution timed out.");
            res.statusCode = 500;
            res.setHeader("Content-Type", "text/plain");
            res.setHeader("Content-Length", 0);
            res.end("");
          }
        }, req.safeTimeout * 1000);

        originalEmit.call(this, event, ...emitArgs);
      } else {
        // Forward to original handler
        originalEmit.call(this, event, ...emitArgs);
      }
    });
  };
}

// ESM and CommonJS override for HTTP servers
new Hook(["http", "https"], wrapHttp);
ritm(["http", "https"], wrapHttp);

// Ensures injections are complete
await waitForAllMessagesAcknowledged();

console.log(
  `Nuxt server starting on PORT=${process.env.PORT} and HOST=${process.env.HOST}`,
);
