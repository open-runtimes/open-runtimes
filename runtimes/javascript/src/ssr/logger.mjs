import { Buffer } from "node:buffer";
import { appendFileSync } from "node:fs";
import { AsyncLocalStorage } from "node:async_hooks";
import { createRequire } from "node:module";

// Where the ESM-only superjson can be loaded synchronously (Node with
// require(esm), Bun), it is loaded lazily on the first structured log so the
// import stays off the SSR boot path. Everywhere else it is awaited eagerly
// at module load, exactly as before this optimization — either way no log is
// ever serialized without it.
const require = createRequire(import.meta.url);
let _superjson;
const superjson = () => {
  if (!_superjson) {
    const module = require("superjson");
    _superjson = module.default ?? module;
  }
  return _superjson;
};
if (!process.features?.require_module && typeof Bun === "undefined") {
  _superjson = (await import("superjson")).default;
}

// Shared between node and bun. Uses AsyncLocalStorage (built into Node 16+
// and Bun) instead of cls-hooked so both runtimes can consume the same file.
export const loggingNamespace = new AsyncLocalStorage();

const isDevelopment = process.env.OPEN_RUNTIMES_ENV === "development";
// Inline logs travel on the response, so no log files are written
export const inlineLogs = process.env.OPEN_RUNTIMES_LOGGING === "inline";
const logsDirectory = inlineLogs
  ? ""
  : (process.env.OPEN_RUNTIMES_LOGS_DIRECTORY ?? "/mnt/logs");

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
      id = isDevelopment ? "dev" : Logger.generateId();
    }

    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG, inline = null) {
    const stringLog = messages
      .map((message) => {
        if (message instanceof Error) {
          return message.stack || String(message);
        }
        if (typeof message === "string") {
          return message;
        }
        try {
          const serializer = superjson();
          return serializer
            ? JSON.stringify(serializer.serialize(message).json)
            : JSON.stringify(message);
        } catch {
          return String(message);
        }
      })
      .join(" ");

    // Each sink is guarded on its own so one failing never drops the other
    try {
      (type === Logger.TYPE_ERROR ? process.stderr : process.stdout).write(
        stringLog + "\n",
      );
    } catch {
      // Silently ignore write failures to prevent runtime crashes.
    }

    if (inline) {
      inline.write(type, stringLog + "\n");
      return;
    }

    try {
      if (logsDirectory && id) {
        appendFileSync(
          `${logsDirectory}/${id}_${type === Logger.TYPE_ERROR ? "errors" : "logs"}.log`,
          stringLog + "\n",
        );
      }
    } catch {
      // Silently ignore write failures to prevent runtime crashes.
    }
  }

  static overrideNativeLogs(namespace, _rid) {
    const forward =
      (type) =>
      (...args) => {
        const store = namespace.getStore();
        Logger.write(store?.id ?? "", args, type, store?.inline);
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

// One byte past edge's limit, so edge sees the overflow and truncates as it does for files
const INLINE_LIMIT = 5 * 1024 * 1024 + 1;

class CappedBuffer {
  chunks = [];
  length = 0;

  write(text) {
    const chunk = Buffer.from(text).subarray(0, INLINE_LIMIT - this.length);
    this.chunks.push(chunk);
    this.length += chunk.length;
  }

  toBuffer() {
    return Buffer.concat(this.chunks);
  }
}

// A request's logs held for its response, with OPEN_RUNTIMES_LOGGING=inline
export class InlineLogs {
  logs = new CappedBuffer();
  errors = new CappedBuffer();

  write(type, text) {
    (type === Logger.TYPE_ERROR ? this.errors : this.logs).write(text);
  }

  take() {
    return [this.logs.toBuffer(), this.errors.toBuffer()];
  }
}
