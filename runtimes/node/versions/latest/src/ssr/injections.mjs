// Node's SSR injection entrypoint — loaded via NODE_OPTIONS='--import ...'.
// All per-request logic lives in the shared http-injection.mjs. Like bun's
// --preload entrypoint, --import fires before any user code runs, so the
// http/https Server prototypes are patched directly instead of hooking every
// module load with import-in-the-middle / require-in-the-middle — the loader
// thread spin-up and per-module source transform of those hooks roughly
// doubled SSR server boot.

import http from "node:http";
import https from "node:https";
import { overrideEmit } from "./http-injection.mjs";

console.log("Preparing SSR runtime ...");

for (const mod of [http, https]) {
  // Patch Server.prototype.emit so every instance — created via
  // createServer(), new Server(), or a subclass — flows through the wrapped
  // emit regardless of how the module was imported.
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
