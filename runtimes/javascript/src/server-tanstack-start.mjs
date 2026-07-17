import handler from "./server/server.js";
import { toNodeHandler } from "srvx/node";
import express from "express";

console.log("TanStack Start server starting ...");

const app = express();

const cacheHeader =
  process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control";

// framework-specific logic
app.use(
  express.static("client", {
    setHeaders: (res, _path) => {
      res.setHeader(cacheHeader, "public, max-age=36000");
    },
  }),
);
const nodeHandler = toNodeHandler(handler.fetch);
app.use(nodeHandler);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`TanStack Start server started on http://${host}:${port}`);
});
