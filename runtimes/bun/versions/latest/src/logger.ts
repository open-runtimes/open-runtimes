const fs = require("node:fs");
import superjson from "superjson";

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  id = "";
  enabled = false;
  includesNativeInfo = false;

  streamLogs: null | fs.WriteStream = null;
  streamErrors: null | fs.WriteStream = null;
  nativeLogsCache: { [key: string]: any } = {};

  constructor(status, id) {
    this.enabled = (status ? status : "enabled") === "enabled";

    if (this.enabled) {
      this.id = id
        ? id
        : process.env.OPEN_RUNTIMES_ENV === "development"
          ? "dev"
          : this.generateId();
    }
  }

  private ensureStreams() {
    if (this.streamLogs === null) {
      this.streamLogs = fs.createWriteStream(`/mnt/logs/${this.id}_logs.log`, {
        flags: "a",
      });
    }
    if (this.streamErrors === null) {
      this.streamErrors = fs.createWriteStream(
        `/mnt/logs/${this.id}_errors.log`,
        {
          flags: "a",
        },
      );
    }
  }

  write(messages: any[], type = Logger.TYPE_LOG, native = false) {
    if (!this.enabled) {
      return;
    }

    if (native && !this.includesNativeInfo) {
      this.includesNativeInfo = true;
      this.write([
        "Native logs detected. Use context.log() or context.error() for better experience.",
      ]);
    }

    this.ensureStreams();

    const stream =
      type === Logger.TYPE_ERROR ? this.streamErrors : this.streamLogs;

    if (!stream) {
      return;
    }

    let stringLog = messages
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

    if (stringLog.length > 8000) {
      stringLog = stringLog.substring(0, 8000);
      stringLog += "... Log truncated due to size limit (8000 characters)";
    }

    try {
      stream.write(stringLog + "\n");
    } catch (error) {
      // Silently fail to prevent 500 errors in runtime
      // Log write failures should not crash the runtime
    }
  }

  async end() {
    if (!this.enabled) {
      return;
    }

    this.enabled = false;

    const promises: Promise<void>[] = [];
    if (this.streamLogs !== null) {
      promises.push(
        new Promise((res) => {
          this.streamLogs?.end(undefined, undefined, res);
        }),
      );
    }
    if (this.streamErrors !== null) {
      promises.push(
        new Promise((res) => {
          this.streamErrors?.end(undefined, undefined, res);
        }),
      );
    }

    await Promise.all(promises);
  }

  overrideNativeLogs() {
    if (!this.enabled) {
      return;
    }

    this.nativeLogsCache.stdlog = console.log.bind(console);
    this.nativeLogsCache.stderror = console.error.bind(console);
    this.nativeLogsCache.stdinfo = console.info.bind(console);
    this.nativeLogsCache.stddebug = console.debug.bind(console);
    this.nativeLogsCache.stdwarn = console.warn.bind(console);

    console.log =
      console.info =
      console.debug =
      console.warn =
      console.error =
        (...args: any[]) => {
          this.write(args, Logger.TYPE_LOG, true);
        };
  }

  revertNativeLogs() {
    if (!this.enabled) {
      return;
    }

    console.log = this.nativeLogsCache.stdlog;
    console.error = this.nativeLogsCache.stderror;
    console.debug = this.nativeLogsCache.stddebug;
    console.warn = this.nativeLogsCache.stdwarn;
    console.info = this.nativeLogsCache.stdinfo;
  }

  // Recreated from https://www.php.net/manual/en/function.uniqid.php
  generateId(padding = 7) {
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
