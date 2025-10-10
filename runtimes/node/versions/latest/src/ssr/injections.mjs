import { Hook, createAddHookMessageChannel } from "import-in-the-middle";
import ritm from "require-in-the-middle";
import { register } from "node:module";

console.log(`Preparing SSR runtime ...`);

const { registerOptions, waitForAllMessagesAcknowledged } =
  createAddHookMessageChannel();
register("import-in-the-middle/hook.mjs", import.meta.url, registerOptions);

function wrapHttp(exports) {
  // Nitro-specific wrapping
  if (exports.Server) {
    exports.Server = wrapServer(exports.Server);
  }

  // Common wrapper (Http, Https, Express.js, ...)
  if (exports.createServer) {
    exports.createServer = wrapCreateServer(exports.createServer);
  }

  return exports;
}

// Override useful for Nitro
function wrapServer(original) {
  return function (...args) {
    const server = original.apply(this, args);

    // TODO: OPR-specific overrides

    return server;
  };
}

// Override for Http, https, express.js ...
function wrapCreateServer(original) {
  return function (...args) {
    const server = new original(...args);

    // TODO: OPR-specific overrides

    return server;
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
