import { handler } from "./server/entry.mjs";
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

console.log("Astro server starting ...");

const app = express();
app.use(onInit);
app.use(express.static("client")); // Framework-specific
app.use(beforeAction);
app.use(onAction(handler)); // Framework-specific
app.use(onError);
app.use(afterAction);

app.listen(getPort(), getHost(), () => {
  console.log(`Astro server started on http://${getHost()}:${getPort()}`);
});
