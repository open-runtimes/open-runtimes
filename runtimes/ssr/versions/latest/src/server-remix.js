import express from "express";
import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";
import "./../logger.js";

console.log("Remix server starting ...");

let app = express();

// Auth check
app.use((req, res, next) => {
  if (
    process.env["OPEN_RUNTIMES_SECRET"] &&
    req.headers[`x-open-runtimes-secret`] !==
      process.env["OPEN_RUNTIMES_SECRET"]
  ) {
    res.writeHead(500, { "Content-Type": "text/plain" });
    res.end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    return;
  }

  next();
});

// TODO: Logging

// SSR handling
app.use(express.static("public"));
app.all(
  "*",
  createRequestHandler({
    build,
    getLoadContext(req, res) {
      return {};
    },
  }),
);

// Port listening
let port = process.env.PORT || 3000;
let host = process.env.HOST || "0.0.0.0";

app.listen(port, host, () => {
  console.log(`Remix server started on http://${host}:${port}`);
});
