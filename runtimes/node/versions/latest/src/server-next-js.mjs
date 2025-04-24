import { parse } from "url";
import next from "next";
import express from "express";
import { onInit, getPort, getHost, onAction, onError, beforeAction, afterAction } from "./ssr/helpers.js";
import { Logger } from "./ssr/logger.js";

console.log("Next.js server starting ...");

const app = express();
app.use(onInit);
app.use(beforeAction);
const nextApp = next({});
const handle = nextApp.getRequestHandler();
app.use(
  onAction((req, res, next) => {
    const parsedUrl = parse(req.url, true);
    handle(req, res, parsedUrl);
  }),
); // Framework-specific
app.use(afterAction);
app.use(onError);

nextApp.prepare().then(() => {
  app.listen(getPort(), getHost(), () => {
    console.log(`Next.js server started on http://${getHost()}:${getPort()}`);
  });
});
