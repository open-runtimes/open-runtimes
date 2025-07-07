import { reqHandler } from "./server/server.mjs";
import express from "express";
import { readFileSync } from "fs";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";

console.log("Angular server starting ...");

const app = express();

app.use((req, res, next) => {
  if (req.headers["x-open-runtimes-timings"]) {
    const timings = readFileSync("/usr/local/telemetry/timings.txt", "utf8");
    res.setHeader("content-type", "text/plain; charset=utf-8");
    return res.status(200).send(timings);
  }
  next();
});

app.use(onInit);

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
