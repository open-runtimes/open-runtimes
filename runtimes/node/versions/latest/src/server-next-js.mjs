import { parse } from "url";
import next from "next";
import express from "express";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("Next.js server starting ...");

const app = express();

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
  app.listen(process.env.PORT, process.env.HOST, () => {
    console.log(`Next.js server started on http://${getHost()}:${getPort()}`);
  });
});
