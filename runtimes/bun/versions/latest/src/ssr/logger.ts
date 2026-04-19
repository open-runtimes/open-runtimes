import { appendFileSync } from "node:fs";
import { AsyncLocalStorage } from "node:async_hooks";
import superjson from "superjson";

// AsyncLocalStorage replaces cls-hooked (which uses node:async_hooks APIs that
// Bun supports through AsyncLocalStorage directly — no need for the wrapper).
export const loggingNamespace = new AsyncLocalStorage<{ id: string }>();

export const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static start(status: string | null | undefined, id: string | null | undefined): string {
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

  static write(id: string, messages: any[], type: string = Logger.TYPE_LOG) {
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

  static overrideNativeLogs(namespace: AsyncLocalStorage<{ id: string }>, _rid: string) {
    const forward = (type: string) => (...args: any[]) => {
      const store = namespace.getStore();
      const requestId = store?.id ?? "";
      Logger.write(requestId, args, type);
    };

    console.log = console.info = console.debug = console.warn = forward(Logger.TYPE_LOG);
    console.error = forward(Logger.TYPE_ERROR);
  }

  // Mirrors node runtime: PHP uniqid-style identifier.
  static generateId(padding: number = 7): string {
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
