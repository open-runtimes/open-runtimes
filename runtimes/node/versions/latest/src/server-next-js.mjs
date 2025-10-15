import { parse } from "url";
import next from "next";
import express from "express";

console.log("Next.js server starting ...");

const app = express();

// framework-specific logic
const nextApp = next({});
const handle = nextApp.getRequestHandler();
app.use((req, res, next) => {
  const parsedUrl = parse(req.url, true);
  handle(req, res, parsedUrl);
});
// End of framework-specific logic

nextApp.prepare().then(() => {
  const port = +(process.env.PORT || "3000");
  const host = process.env.HOST || "0.0.0.0";
  app.listen(port, host, () => {
    console.log(`Next.js server started on http://${host}:${port}`);
  });
});
