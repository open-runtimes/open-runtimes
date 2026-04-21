// Bun's SSR injection entrypoint — loaded via `bun --preload ...`.
// All per-request logic lives in the shared
// runtimes/javascript/src/ssr/http-injection.mjs (overlaid into
// /usr/local/server/src/ssr/ at image build time). This file only differs from
// node in *how* the wrapper reaches the http module: since --preload fires
// before any user code, we can patch http.Server.prototype directly rather
// than hooking every module load with import-in-the-middle /
// require-in-the-middle.

import http from "node:http";
import https from "node:https";
import { overrideEmit } from "./http-injection.mjs";

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
}

console.log(
  `SSR runtime prepared with configuration PORT=${process.env.PORT} and HOST=${process.env.HOST}`,
);
