import { appendFileSync } from "node:fs";
import { readFile } from "node:fs/promises";

const TIMINGS_PATH = "/mnt/telemetry/timings.txt";

// Monotonic offset (seconds since process start) when the SSR server socket
// started listening; also gates the one-shot first_request stamp.
let listenedAt;
let firstRequestPending = true;

function recordTiming(key, seconds) {
  try {
    appendFileSync(TIMINGS_PATH, `${key}=${seconds.toFixed(6)}\n`);
  } catch {
    // Telemetry must never break the runtime.
  }
}

// Called on the wrapped server's "listening" event. Emits boot milestones as
// offsets from process start (seconds) and the wall-clock anchor for the
// process timeline. nodeTiming is Node-only (absent on bun); milestones of -1
// mean "not reached" and are skipped so a missing key signals that.
export function recordServerListening() {
  if (listenedAt !== undefined) {
    return;
  }
  listenedAt = performance.now();

  const nodeTiming = performance.nodeTiming;
  if (nodeTiming) {
    if (nodeTiming.bootstrapComplete >= 0) {
      recordTiming("node_boot", nodeTiming.bootstrapComplete / 1000);
    }
    if (nodeTiming.environment >= 0) {
      recordTiming("env_ready", nodeTiming.environment / 1000);
    }
  }
  recordTiming("listen", listenedAt / 1000);
  recordTiming("anchor_node_wall", performance.timeOrigin / 1000);
}

// Stamp listen → first response on the first real request (the /__opr
// endpoints return before this is installed). Wraps writeHead so the value is
// measured when the response is ready — after any first-render/route-compile
// work — and can still travel as a response header to the executor.
export function addFirstRequestTiming(req, res) {
  if (!firstRequestPending || listenedAt === undefined) {
    return;
  }
  firstRequestPending = false;

  const originalWriteHead = res.writeHead;
  res.writeHead = function (...args) {
    const firstRequest = (performance.now() - listenedAt) / 1000;
    recordTiming("first_request", firstRequest);
    try {
      if (!res.headersSent) {
        res.setHeader(
          "x-open-runtimes-timings",
          `first_request=${firstRequest.toFixed(6)}`,
        );
      }
    } catch {
      // Telemetry must never break the response.
    }
    return originalWriteHead.apply(this, args);
  };
}

export function addOprEndpoints(req, res) {
  if (req.method === "GET" && req.url === "/__opr/health") {
    const body = "OK";

    res.statusCode = 200;
    res.setHeader("Content-Type", "text/plain");
    res.setHeader("Content-Length", Buffer.byteLength(body));
    res.end(body);
    return true;
  } else if (req.method === "GET" && req.url === "/__opr/timings") {
    (async () => {
      const body = await readFile("/mnt/telemetry/timings.txt", "utf8");

      res.statusCode = 200;
      res.setHeader("Content-Type", "text/plain");
      res.setHeader("Content-Length", Buffer.byteLength(body));
      res.end(body);
    })();
    return true;
  }

  return false;
}

export function addAuthenticationCheck(req, res) {
  const serverSecret = process.env["OPEN_RUNTIMES_SECRET"];
  const headerSecret = req.headers["x-open-runtimes-secret"];

  if (serverSecret && serverSecret !== headerSecret) {
    const body =
      'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    res.statusCode = 500;
    res.setHeader("Content-Type", "text/plain");
    res.setHeader("Content-Length", Buffer.byteLength(body));
    res.end(body);
    return true;
  }

  return false;
}

export function addSafeTimeout(req, res) {
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
      return true;
    }

    safeTimeout = +timeout;
  }

  req.safeTimeout = safeTimeout;

  return false;
}
