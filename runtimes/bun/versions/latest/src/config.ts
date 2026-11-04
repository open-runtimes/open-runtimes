let headers: Record<string, string> = {};
try {
  headers = JSON.parse(Bun.env["OPEN_RUNTIMES_HEADERS"] ?? "{}");
} catch {}

// Inline logs travel on the response, so no log files are written
const inlineLogs = Bun.env["OPEN_RUNTIMES_LOGGING"] === "inline";

export const config = {
  secret: Bun.env["OPEN_RUNTIMES_SECRET"] ?? "",
  headers,
  entrypoint: Bun.env["OPEN_RUNTIMES_ENTRYPOINT"] ?? "",
  env: Bun.env["OPEN_RUNTIMES_ENV"] ?? "",
  inlineLogs,
  logsDirectory: inlineLogs
    ? ""
    : (Bun.env["OPEN_RUNTIMES_LOGS_DIRECTORY"] ?? "/mnt/logs"),
};
