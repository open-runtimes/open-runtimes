import { reqHandler } from "./server/server.mjs";
import express from "express";
import "./../logger.js";

console.log("Angular server starting ...");

const app = express();

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

// SSR handling
app.use(reqHandler);

// Port listening
const port = parseInt(process.env.PORT || "3000", 10);
const host = process.env.HOST || "0.0.0.0";

app.listen(port, host, () => {
  console.log(`Angular server started on http://${host}:${port}`);
});
