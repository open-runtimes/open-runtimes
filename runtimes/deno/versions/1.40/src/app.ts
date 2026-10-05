import { Application } from "https://deno.land/x/oak@v17.0.0/mod.ts";
import { Server } from "https://deno.land/x/oak@v17.0.0/http_server_native.ts";
import { gracefulServer } from "./gracefulServer.ts";

export function createApp() {
  return new Application({ serverConstructor: gracefulServer(Server) });
}
