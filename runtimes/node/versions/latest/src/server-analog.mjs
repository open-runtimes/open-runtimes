import { handler } from "./server/index.mjs";
import express from "express";
import {
  onInit,
  getPort,
  getHost,
  onAction,
  onError,
  telemetryMiddleware,
} from "./ssr/helpers.mjs";

console.log("Analog server starting ...");

const app = express();

app.use(telemetryMiddleware);
app.use(onInit);

// framework-specific logic
app.use(express.static("public"));
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Analog server started on http://${getHost()}:${getPort()}`);
});
