let headers: Record<string, string> = {};
try {
  headers = JSON.parse(Bun.env["OPEN_RUNTIMES_HEADERS"] ?? "{}");
} catch {}

export const config = {
  secret: Bun.env["OPEN_RUNTIMES_SECRET"] ?? "",
  headers,
  entrypoint: Bun.env["OPEN_RUNTIMES_ENTRYPOINT"] ?? "",
  env: Bun.env["OPEN_RUNTIMES_ENV"] ?? "",
};
