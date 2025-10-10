// Use as base for server-X.js (per framework)
import { Logger, loggingNamespace } from "./logger.mjs";

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
