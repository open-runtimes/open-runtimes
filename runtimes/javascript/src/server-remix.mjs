import installShutdown from "/usr/local/server/helpers/http-shutdown.cjs";
import * as build from "./build/server/index.js";
import express from "express";
import { readFileSync } from "node:fs";

console.log("Remix server starting ...");

const { dependencies = {} } = JSON.parse(readFileSync("package.json", "utf8"));
const reactRouter =
  !dependencies["@remix-run/express"] &&
  Object.keys(dependencies).some(
    (name) => name === "react-router" || name.startsWith("@react-router/"),
  );
const { createRequestHandler } = reactRouter
  ? await import("@react-router/express")
  : await import("@remix-run/express");

const app = express();

const cacheHeader =
  process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control";

// framework-specific logic
app.use(
  express.static("build/client", {
    setHeaders: (res, _path) => {
      res.setHeader(cacheHeader, "public, max-age=36000");
    },
  }),
);
// React Router 8 rejects a load context that is not a RouterContextProvider
app.use(createRequestHandler({ build }));
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
installShutdown(
  app.listen(port, host, () => {
    console.log(`Remix server started on http://${host}:${port}`);
  }),
);
