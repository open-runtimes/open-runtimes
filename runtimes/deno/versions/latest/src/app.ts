import { Application } from "jsr:@oak/oak@17.1.6/application";
import { Server } from "jsr:@oak/oak@17.1.6/http_server_native";
import { gracefulServer } from "./gracefulServer.ts";

export function createApp() {
  return new Application({ serverConstructor: gracefulServer(Server) });
}
