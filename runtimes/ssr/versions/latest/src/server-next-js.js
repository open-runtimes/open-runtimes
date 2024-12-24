import { createServer } from "http";
import { parse } from "url";
import next from "next";
import "./../logger.js";

console.log("Next.js server starting ...");

const app = next({});
const handle = app.getRequestHandler();

app.prepare().then(() => {
  const server = createServer((req, res) => {
    // Auth check
    if (
      process.env["OPEN_RUNTIMES_SECRET"] &&
      req.headers[`x-open-runtimes-secret`] !==
        process.env["OPEN_RUNTIMES_SECRET"]
    ) {
      res.writeHead(500, { "Content-Type": "text/plain" });
      res.end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
      return;
    }

    // SSR handling
    const parsedUrl = parse(req.url, true);
    handle(req, res, parsedUrl);
  });

  // Port listening
  const port = parseInt(process.env.PORT || "3000", 10);
  const host = process.env.HOST || "0.0.0.0";
  server.listen(port, host);

  console.log(`Next.js server started on http://${host}:${port}`);
});
