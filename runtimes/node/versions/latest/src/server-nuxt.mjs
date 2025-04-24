import { listener } from "./server/index.mjs";
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

console.log("Nuxt server starting ...");

const app = express();
app.use(onInit);
app.use(express.static("public")); // Framework-specific
app.use(beforeAction);
app.use(onAction(listener)); // Framework-specific
app.use(onError);
app.use(afterAction);

app.listen(getPort(), getHost(), () => {
  console.log(`Nuxt server started on http://${getHost()}:${getPort()}`);
});
