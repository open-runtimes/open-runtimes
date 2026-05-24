// Node's SSR injection entrypoint — loaded via NODE_OPTIONS='--import ...'.
// All per-request logic lives in the shared http-injection.mjs; this file only
// wires the wrappers up using node-specific hook mechanisms (import-in-the-
// middle for ESM, require-in-the-middle for CJS) so every load of http/https
// receives the wrapped Server / createServer.

import { Hook, createAddHookMessageChannel } from "import-in-the-middle";
import ritm from "require-in-the-middle";
import { register } from "node:module";
import { wrapHttp } from "./http-injection.mjs";

console.log("Preparing SSR runtime ...");

const { registerOptions, waitForAllMessagesAcknowledged } =
  createAddHookMessageChannel();
register("import-in-the-middle/hook.mjs", import.meta.url, registerOptions);

new Hook(["http", "https"], wrapHttp);
ritm(["http", "https"], wrapHttp);

await waitForAllMessagesAcknowledged();

console.log(
  `SSR runtime prepared with configuration PORT=${process.env.PORT} and HOST=${process.env.HOST}`,
);
