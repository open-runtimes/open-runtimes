import { handler } from "./server/entry.mjs";
import { createServer } from "node:http";
import serveStatic from "serve-static";

console.log("Astro server starting ...");

const cacheHeader =
  process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control";

// framework-specific logic
const staticHandler = serveStatic("client", {
  setHeaders: (res, _path) => {
    res.setHeader(cacheHeader, "public, max-age=36000");
  },
});

// Terminal handler: mirror express's finalhandler for requests the framework
// hands back (404) or fails on (500).
const finish = (res) => (error) => {
  res.statusCode = error ? 500 : 404;
  res.end();
};
const server = createServer((req, res) => {
  staticHandler(req, res, () => handler(req, res, finish(res)));
});
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
server.listen(port, host, () => {
  console.log(`Astro server started on http://${host}:${port}`);
});
