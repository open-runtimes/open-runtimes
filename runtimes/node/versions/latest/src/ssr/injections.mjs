import { Hook, createAddHookMessageChannel } from "import-in-the-middle";
import ritm from "require-in-the-middle";
import { register } from "node:module";
import { handleOprEndpoints } from "./injections/system.mjs";

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

    // Health check, telemetry, etc.
    handleOprEndpoints(req, res);
    if (res.writableEnded) {
      return;
    }

    // Forward to original handler
    originalEmit.call(this, event, ...emitArgs);
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
