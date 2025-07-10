import { parse } from "url";
import next from "next";
import express from "express";
import { readFileSync } from "fs";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";

console.log("Next.js server starting ...");

const app = express();

app.use((req, res, next) => {
  if (req.headers["x-open-runtimes-timings"]) {
    const timings = readFileSync("/mnt/telemetry/timings.txt", "utf8");
    res.setHeader("content-type", "text/plain; charset=utf-8");
    return res.status(200).send(timings);
  }
  next();
});

app.use(onInit);

// framework-specific logic
const nextApp = next({});
const handle = nextApp.getRequestHandler();
app.use(
  onAction((req, res, next) => {
    const parsedUrl = parse(req.url, true);
    handle(req, res, parsedUrl);
  }),
);
// End of framework-specific logic

app.use(onError);

nextApp.prepare().then(() => {
  app.listen(getPort(), getHost(), () => {
    console.log(`Next.js server started on http://${getHost()}:${getPort()}`);
  });
});
