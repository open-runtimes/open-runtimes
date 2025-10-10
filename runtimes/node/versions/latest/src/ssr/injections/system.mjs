import { readFile } from "node:fs/promises";

export async function handleOprEndpoints(req, res) {
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
