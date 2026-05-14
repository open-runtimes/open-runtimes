// Deno's SSR injection entrypoint — loaded via `deno run --import=...`.
// All per-request logic lives in the shared
// runtimes/javascript/src/ssr/http-injection.mjs (overlaid into
// /usr/local/server/src/ssr/ at image build time). Like bun, Deno's `node:http`
// polyfill lets us patch Server.prototype.emit directly when the preload module
// runs first, rather than hooking every module load the way node does with
// import-in-the-middle / require-in-the-middle.

import http from "node:http";
import https from "node:https";
import { overrideEmit } from "./http-injection.mjs";

console.log("Preparing SSR runtime ...");

for (const mod of [http, https] as any[]) {
  if (!mod) continue;

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

const port = Deno.env.get("PORT");
const host = Deno.env.get("HOST");
console.log(
  `SSR runtime prepared with configuration PORT=${port} and HOST=${host}`,
);
