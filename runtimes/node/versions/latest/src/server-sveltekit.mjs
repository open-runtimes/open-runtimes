import { handler } from "./handler.js";
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

console.log("SvelteKit server starting ...");

const app = express();
app.use(onInit);
app.use(beforeAction);
app.use(onAction(handler)); // Framework-specific
app.use(afterAction);
app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`SvelteKit server started on http://${getHost()}:${getPort()}`);
});
