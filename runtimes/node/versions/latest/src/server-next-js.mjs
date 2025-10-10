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
  app.listen(
    +(process.env.PORT || "3000"),
    process.env.HOST || "0.0.0.0",
    () => {
      console.log(`Next.js server started.`);
    },
  );
});
