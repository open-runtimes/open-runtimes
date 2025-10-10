import express from "express";
import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("Remix server starting ...");

const app = express();

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

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`Remix server started on http://${getHost()}:${getPort()}`);
});
