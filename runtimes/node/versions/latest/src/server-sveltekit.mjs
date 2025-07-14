import { handler } from "./handler.js";
import express from "express";
import {
  onInit,
  getPort,
  getHost,
  onAction,
  onError,
  telemetryMiddleware,
} from "./ssr/helpers.mjs";

console.log("SvelteKit server starting ...");

const app = express();

app.use(telemetryMiddleware);

app.use(onInit);

// framework-specific logic
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`SvelteKit server started on http://${getHost()}:${getPort()}`);
});
