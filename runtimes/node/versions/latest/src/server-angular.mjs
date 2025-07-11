import {
  onInit,
  getPort,
  getHost,
  onAction,
  onError,
  telemetryMiddleware,
} from "./ssr/helpers.mjs";
import express from "express";

console.log("Angular server starting ...");

const app = express();

app.use(telemetryMiddleware);

app.use(onInit);

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
