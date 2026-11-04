function parseHeaders(): Record<string, unknown> {
  try {
    return JSON.parse(Deno.env.get("OPEN_RUNTIMES_HEADERS") || "{}");
  } catch {
    return {};
  }
}

// Inline logs travel on the response, so no log files are written
const inlineLogs = Deno.env.get("OPEN_RUNTIMES_LOGGING") === "inline";

export const config = {
  secret: Deno.env.get("OPEN_RUNTIMES_SECRET") ?? "",
  headers: parseHeaders(),
  entrypoint: Deno.env.get("OPEN_RUNTIMES_ENTRYPOINT") ?? "",
  env: Deno.env.get("OPEN_RUNTIMES_ENV") ?? "",
  inlineLogs,
  logsDirectory: inlineLogs
    ? ""
    : (Deno.env.get("OPEN_RUNTIMES_LOGS_DIRECTORY") ?? "/mnt/logs"),
};
