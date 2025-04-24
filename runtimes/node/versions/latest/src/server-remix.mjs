import express from "express";
import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";
import {
  onInit,
  getPort,
  getHost,
  onAction,
  onError,
  beforeAction,
  afterAction,
} from "./ssr/helpers.js";

console.log("Remix server starting ...");

const app = express();
app.use(onInit);
app.use(express.static("build/client")); // Framework-specific
app.use(beforeAction);
app.all(
  "*",
  onAction(
    createRequestHandler({
      build,
      getLoadContext(req, res) {
        return {};
      },
    }),
  ),
); // Framework-specific
app.use(onError);
app.use(afterAction);

app.listen(getPort(), getHost(), () => {
  console.log(`Remix server started on http://${getHost()}:${getPort()}`);
});
