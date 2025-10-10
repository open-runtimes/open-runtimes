// Use as base for server-X.js (per framework)
import { Logger, loggingNamespace } from "./logger.mjs";

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
          return;
        }
      } else {
        await callback(...params);
      }
    });
  };
}

// When error occurs
export function onError(error, req, res, next) {
  if (res.headersSent) {
    return next(error);
  }

  Logger.write(req.loggerId, [error], Logger.TYPE_ERROR);
  res.writeHead(500, { "Content-Type": "text/plain" });
  res.end("");
  return;
}
