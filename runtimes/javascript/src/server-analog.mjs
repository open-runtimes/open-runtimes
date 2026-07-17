import { handler } from "./server/index.mjs";
import express from "express";

console.log("Analog server starting ...");

const app = express();

const cacheHeader =
  process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control";

// framework-specific logic
app.use(
  express.static("public", {
    setHeaders: (res, _path) => {
      res.setHeader(cacheHeader, "public, max-age=36000");
    },
  }),
);
app.use(handler);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`Analog server started on http://${host}:${port}`);
});
