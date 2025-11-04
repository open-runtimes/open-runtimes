import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";
import express from "express";

console.log("Remix server starting ...");

const app = express();

// framework-specific logic
app.use(
  express.static("build/client", {
    setHeaders: (res, path) => {
      res.setHeader(
        process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control",
        "public, max-age=36000",
      );
    },
  }),
);
app.all(
  "*",
  createRequestHandler({
    build,
    getLoadContext(req, res) {
      return {};
    },
  }),
);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`Remix server started on http://${host}:${port}`);
});
