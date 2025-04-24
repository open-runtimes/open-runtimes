import { handler } from "./server/index.mjs";
import express from "express";
import {
  onInit,
  getPort,
  getHost,
  onAction,
  onError,
  beforeAction,
  afterAction,
} from "./ssr/helpers.js";

console.log("Analog server starting ...");

const app = express();
app.use(onInit);
app.use(express.static("public")); // Framework-specific
app.use(beforeAction);
app.use(onAction(handler)); // Framework-specific
app.use(afterAction);
app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Analog server started on http://${getHost()}:${getPort()}`);
});
