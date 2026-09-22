import installShutdown from "/usr/local/server/helpers/http-shutdown.cjs";
import { parse } from "url";
import next from "next";
import { createServer } from "node:http";
import serveStatic from "serve-static";

console.log("Next.js server starting ...");

const cacheHeader =
  process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control";

// framework-specific logic
const staticHandler = serveStatic("public", {
  setHeaders: (res, _path) => {
    res.setHeader(cacheHeader, "public, max-age=36000");
  },
});
const nextApp = next({});
const handle = nextApp.getRequestHandler();
const server = createServer((req, res) => {
  staticHandler(req, res, () => {
    const parsedUrl = parse(req.url, true);
    handle(req, res, parsedUrl);
  });
});
installShutdown(server);
// End of framework-specific logic

nextApp.prepare().then(() => {
  const port = +(process.env.PORT || "3000");
  const host = process.env.HOST || "0.0.0.0";
  server.listen(port, host, () => {
    console.log(`Next.js server started on http://${host}:${port}`);
  });
});
