// Bun's SSR injection entrypoint — loaded via BUN_OPTIONS='--preload ...'.
// All per-request logic and the wrapServer / wrapCreateServer / wrapHttp
// helpers live in the shared runtimes/javascript/src/ssr/http-injection.mjs
// (overlaid into /usr/local/server/src/ssr/ at image build time). This file
// only differs from node in *how* the wrappers reach the http module: since
// --preload fires before any user code, we can patch http.Server.prototype
// and http.createServer directly rather than hooking every module load with
// import-in-the-middle / require-in-the-middle.

import http from "node:http";
import https from "node:https";
import {
  overrideEmit,
  wrapCreateServer,
  wrapServer,
} from "./http-injection.mjs";

console.log("Preparing SSR runtime ...");

for (const mod of [http, https] as any[]) {
  if (!mod) continue;

  // Patch Server.prototype.emit so instances created via `new Server()` —
  // including Bun's internal http compat path — flow through the wrapped emit.
  if (
    mod.Server &&
    mod.Server.prototype &&
    !mod.Server.prototype.__oprEmitPatched
  ) {
    const originalEmit = mod.Server.prototype.emit;
    mod.Server.prototype.emit = overrideEmit(originalEmit);
    mod.Server.prototype.__oprEmitPatched = true;
  }

  // Also wrap the constructors so any future code path that bypasses
  // prototype lookup still gets the Open Runtimes contract.
  if (mod.createServer) {
    mod.createServer = wrapCreateServer(mod.createServer);
  }
  if (mod.Server) {
    mod.Server = wrapServer(mod.Server);
  }
}

console.log(
  `SSR runtime prepared with configuration PORT=${process.env.PORT} and HOST=${process.env.HOST}`,
);
