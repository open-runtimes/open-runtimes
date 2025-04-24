// Use as base for server-X.js (per framework)

import { Logger } from "./logger.js";
import { createNamespace } from "cls-hooked";

const loggingNamespace = createNamespace("logging");

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

// Start logging
export function beforeAction(req, res, next) {
  const loggerId = Logger.start(
    req.headers[`x-open-runtimes-logging`],
    req.headers[`x-open-runtimes-log-id`],
  );
  req.loggerId = loggerId;
  res.setHeader("x-open-runtimes-log-id", loggerId);
  next();
}

// End logging
export async function afterAction(req, res, next) {
  Logger.nativeLog('Writing logs');
  await Logger.end(req.loggerId);
  next();
}

// Wrapper for SSR handling
export function onAction(callback) {
  return async (...params) => {
    loggingNamespace.run(async () => {
      const [req, res, next] = params;

      loggingNamespace.set("id", req.loggerId);
      Logger.overrideNativeLogs(loggingNamespace, req.loggerId);

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
          console.error("Execution timed out.");
          res.writeHead(500, { "Content-Type": "text/plain" });
          res.end("");
          next();
          return;
        }
        next();
      } else {
        await callback(...params);
        next();
      }
    });
  };
}

// When error occurs
export function onError(error, req, res, next) {
  if (res.headersSent) {
    return next(err);
  }

  Logger.write(req.loggerId, [error], Logger.TYPE_ERROR);
  res.writeHead(500, { "Content-Type": "text/plain" });
  res.end("");
  return;
}
