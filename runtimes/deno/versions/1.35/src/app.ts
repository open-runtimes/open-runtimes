import { Application } from "https://deno.land/x/oak@v12.6.1/mod.ts";

// Oak 14 requires HttpServer.shutdown(), unavailable in Deno 1.35.
export function createApp() {
  return new Application();
}
