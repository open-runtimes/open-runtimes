// Use as base for server-X.js (per framework)

import { Logger } from "./logger.js";

export function getPort() {
  const port = parseInt(process.env.PORT || "3000", 10);
  return port;
}

export function getHost() {
  const host = process.env.HOST || "0.0.0.0";
  return host;
}

// Before request starts
export function onInit(req, res, next) {
  // Ensure auth headers
  if (
    process.env["OPEN_RUNTIMES_SECRET"] &&
    req.headers[`x-open-runtimes-secret`] !==
      process.env["OPEN_RUNTIMES_SECRET"]
  ) {
    res.writeHead(500, { "Content-Type": "text/plain" });
    res.end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    return;
  }

  // Setup logging
  req.logger = new Logger(
    req.headers[`x-open-runtimes-logging`],
    req.headers[`x-open-runtimes-log-id`],
  );
  res.setHeader("x-open-runtimes-log-id", req.logger.id);
  res.on("finish", () => {
    req.logger.end();
  });

  // Validate safe timeout
  const timeout = req.headers[`x-open-runtimes-timeout`] ?? "";
  let safeTimeout = null;
  if (timeout) {
    if (isNaN(timeout) || timeout === 0) {
      res.writeHead(500, { "Content-Type": "text/plain" });
      res.end(
        'Header "x-open-runtimes-timeout" must be an integer greater than 0.',
      );
      return;
    }

    safeTimeout = +timeout;
  }
  req.safeTimeout = safeTimeout;

  next();
}

// Wrapper for SSR handling
export function onAction(callback) {
  return async (...params) => {
    const [req, res, next] = params;

    req.logger.overrideNativeLogs();

    if (req.safeTimeout !== null) {
      let executed = true;

      const timeoutPromise = new Promise((promiseRes) => {
        setTimeout(() => {
          executed = false;
          promiseRes(true);
        }, req.safeTimeout * 1000);
      });

      await Promise.race([callback(...params), timeoutPromise]);

      if (!executed) {
        context.error("Execution timed out.");
        res.writeHead(500, { "Content-Type": "text/plain" });
        res.end("");
        return;
      }
    } else {
      await callback(...params);
    }
  };
}

// When error occurs
export function onError(error, req, res) {
  req.logger.write([error], Logger.TYPE_ERROR);
  res.writeHead(500, { "Content-Type": "text/plain" });
  res.end("");
  return;
}
