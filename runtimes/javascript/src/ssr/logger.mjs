import { appendFileSync } from "node:fs";
import { AsyncLocalStorage } from "node:async_hooks";
import superjson from "superjson";

// Shared between node and bun. Uses AsyncLocalStorage (built into Node 16+
// and Bun) instead of cls-hooked so both runtimes can consume the same file.
export const loggingNamespace = new AsyncLocalStorage();

export const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static start(status, id) {
    const enabled = (status ? status : "enabled") === "enabled";

    if (!enabled) {
      return "";
    }

    if (!id) {
      id =
        process.env.OPEN_RUNTIMES_ENV === "development"
          ? "dev"
          : Logger.generateId();
    }

    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG) {
    const stringLog = messages
      .map((message) => {
        if (message instanceof Error) {
          return message.stack || String(message);
        }
        if (typeof message === "string") {
          return message;
        }
        try {
          return JSON.stringify(superjson.serialize(message).json);
        } catch {
          return String(message);
        }
      })
      .join(" ");

    const path = `/mnt/logs/${id}_${type === Logger.TYPE_ERROR ? "errors" : "logs"}.log`;
    try {
      appendFileSync(path, stringLog + "\n");
    } catch {
      // Silently ignore write failures to prevent runtime crashes.
    }
  }

  static overrideNativeLogs(namespace, _rid) {
    const forward = (type) => (...args) => {
      const requestId = namespace.getStore()?.id ?? "";
      Logger.write(requestId, args, type);
    };

    console.log =
      console.info =
      console.debug =
      console.warn =
        forward(Logger.TYPE_LOG);
    console.error = forward(Logger.TYPE_ERROR);
  }

  // Recreated from https://www.php.net/manual/en/function.uniqid.php
  static generateId(padding = 7) {
    const now = new Date();
    const sec = Math.floor(now.getTime() / 1000);
    const msec = now.getMilliseconds();
    const baseId = sec.toString(16) + msec.toString(16).padStart(5, "0");
    let randomPadding = "";
    for (let i = 0; i < padding; i++) {
      const randomHexDigit = Math.floor(Math.random() * 16).toString(16);
      randomPadding += randomHexDigit;
    }
    return baseId + randomPadding;
  }
}
