function parseHeaders(): Record<string, unknown> {
  try {
    return JSON.parse(Deno.env.get("OPEN_RUNTIMES_HEADERS") || "{}");
  } catch {
    return {};
  }
}

export const config = {
  secret: Deno.env.get("OPEN_RUNTIMES_SECRET") ?? "",
  headers: parseHeaders(),
  entrypoint: Deno.env.get("OPEN_RUNTIMES_ENTRYPOINT") ?? "",
  env: Deno.env.get("OPEN_RUNTIMES_ENV") ?? "",
};
