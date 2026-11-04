let headers = {};
try {
  headers = JSON.parse(process.env.OPEN_RUNTIMES_HEADERS || "{}");
} catch {
  // Silently ignore invalid JSON and keep empty enforced headers
}

// Inline logs travel on the response, so no log files are written
const inlineLogs = process.env.OPEN_RUNTIMES_LOGGING === "inline";

module.exports = Object.freeze({
  secret: process.env.OPEN_RUNTIMES_SECRET,
  headers,
  entrypoint: process.env.OPEN_RUNTIMES_ENTRYPOINT,
  env: process.env.OPEN_RUNTIMES_ENV,
  inlineLogs,
  logsDirectory: inlineLogs
    ? ""
    : (process.env.OPEN_RUNTIMES_LOGS_DIRECTORY ?? "/mnt/logs"),
});
