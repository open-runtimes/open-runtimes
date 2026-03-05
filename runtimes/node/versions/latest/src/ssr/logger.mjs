import { appendFileSync } from "fs";
import { createNamespace } from "cls-hooked";
import { uneval } from "devalue";

export const loggingNamespace = createNamespace("logging");

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
        if (typeof message === "object" && message !== null) {
          try {
            return uneval(message);
          } catch {
            return String(message);
          }
        }
        return String(message);
      })
      .join(" ");

    const path = `/mnt/logs/${id}_${type === Logger.TYPE_ERROR ? "errors" : "logs"}.log`;
    try {
      appendFileSync(path, stringLog + "\n");
    } catch (err) {
      // Silently ignore write failures to prevent runtime crashes
      // The logging system should not cause the main execution to fail
    }
  }

  static overrideNativeLogs(namespace, rid) {
    console.log =
      console.info =
      console.debug =
      console.warn =
        (...args) => {
          const requestId = namespace.get("id");
          Logger.write(requestId, args, Logger.TYPE_LOG, true);
        };

    console.error = (...args) => {
      const requestId = namespace.get("id");
      Logger.write(requestId, args, Logger.TYPE_ERROR, true);
    };
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
