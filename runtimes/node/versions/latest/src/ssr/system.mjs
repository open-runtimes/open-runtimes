import { readFile } from "node:fs/promises";

export async function addOprEndpoints(req, res) {
  if (req.method === "GET" && req.url === "/__opr/health") {
    const body = "OK";

    res.statusCode = 200;
    res.setHeader("Content-Type", "text/plain");
    res.setHeader("Content-Length", Buffer.byteLength(body));
    res.end(body);
  } else if (req.method === "GET" && req.url === "/__opr/timings") {
    const body = await readFile("/mnt/telemetry/timings.txt", "utf8");

    res.statusCode = 200;
    res.setHeader("Content-Type", "text/plain");
    res.setHeader("Content-Length", Buffer.byteLength(body));
    res.end(body);
  }
}

export async function addAuthenticationCheck(req, res) {
  const serverSecret = process.env["OPEN_RUNTIMES_SECRET"];
  const headerSecret = req.headers["x-open-runtimes-secret"];

  if (serverSecret && serverSecret !== headerSecret) {
    const body =
      'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    res.statusCode = 500;
    res.setHeader("Content-Type", "text/plain");
    res.setHeader("Content-Length", Buffer.byteLength(body));
    res.end(body);
    return;
  }
}

export async function addSafeTimeout(req, res) {
  const timeout = req.headers[`x-open-runtimes-timeout`] ?? "";
  let safeTimeout = null;
  if (timeout) {
    if (isNaN(timeout) || timeout === 0) {
      const body =
        'Header "x-open-runtimes-timeout" must be an integer greater than 0.';
      res.statusCode = 500;
      res.setHeader("Content-Type", "text/plain");
      res.setHeader("Content-Length", Buffer.byteLength(body));
      res.end(body);
      return;
    }

    safeTimeout = +timeout;
  }

  req.safeTimeout = safeTimeout;
}
