import express from "express";
import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";

console.log("Remix server starting ...");

const app = express();
app.use(onInit);

// framework-specific logic
app.use(express.static("build/client"));
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
);
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Remix server started on http://${getHost()}:${getPort()}`);
});
