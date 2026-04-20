// Deno's SSR injection entrypoint — loaded via `deno run --import=...`.
// All per-request logic and the wrapServer / wrapCreateServer helpers live in
// the shared runtimes/javascript/src/ssr/http-injection.mjs (overlaid into
// /usr/local/server/src/ssr/ at image build time). Like bun, Deno's `node:http`
// polyfill lets us patch Server.prototype.emit + createServer directly when
// the preload module runs first, rather than hooking every module load the way
// node does with import-in-the-middle / require-in-the-middle.

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

  if (
    mod.Server &&
    mod.Server.prototype &&
    !mod.Server.prototype.__oprEmitPatched
  ) {
    const originalEmit = mod.Server.prototype.emit;
    mod.Server.prototype.emit = overrideEmit(originalEmit);
    mod.Server.prototype.__oprEmitPatched = true;
  }

  if (mod.createServer) {
    mod.createServer = wrapCreateServer(mod.createServer);
  }
  if (mod.Server) {
    mod.Server = wrapServer(mod.Server);
  }
}

console.log(
  `SSR runtime prepared with configuration PORT=${Deno.env.get("PORT")} and HOST=${Deno.env.get("HOST")}`,
);
