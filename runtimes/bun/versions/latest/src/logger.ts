const fs = require("node:fs");
import superjson from "superjson";

import { config } from "./config";

export class InlineBuffer {
  chunks: Uint8Array[] = [];
  length = 0;

  constructor(private limit: number) {}

  write(bytes: Uint8Array) {
    const chunk = bytes.subarray(0, this.limit - this.length);
    this.chunks.push(chunk);
    this.length += chunk.length;
  }
}

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";
  // One byte past edge's limit, so edge sees the overflow and truncates as it does for files
  static INLINE_LIMIT = 5 * 1024 * 1024 + 1;

  id = "";
  enabled = false;
  includesNativeInfo = false;
  inline = false;

  streamLogs: null | fs.WriteStream = null;
  streamErrors: null | fs.WriteStream = null;
  nativeLogsCache: { [key: string]: any } = {};
  // Inline mode returns logs on the response instead of writing files
  inlineLogs = new InlineBuffer(Logger.INLINE_LIMIT);
  inlineErrors = new InlineBuffer(Logger.INLINE_LIMIT);

  constructor(status, id, inline = false) {
    this.enabled = (status ? status : "enabled") === "enabled";
    this.inline = inline;

    if (this.enabled) {
      this.id = id
        ? id
        : config.env === "development"
          ? "dev"
          : this.generateId();
      if (config.logsDirectory) {
        this.streamLogs = fs.createWriteStream(
          `${config.logsDirectory}/${this.id}_logs.log`,
          {
            flags: "a",
          },
        );
        this.streamErrors = fs.createWriteStream(
          `${config.logsDirectory}/${this.id}_errors.log`,
          {
            flags: "a",
          },
        );
      }
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

    const stream =
      type === Logger.TYPE_ERROR ? this.streamErrors : this.streamLogs;

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

    // Each sink is guarded on its own so one failing never drops the other
    try {
      (type === Logger.TYPE_ERROR ? process.stderr : process.stdout).write(
        stringLog + "\n",
      );
    } catch (error) {
      // Silently fail to prevent 500 errors in runtime
      // Log write failures should not crash the runtime
    }

    if (this.inline) {
      (type === Logger.TYPE_ERROR ? this.inlineErrors : this.inlineLogs).write(
        Buffer.from(stringLog + "\n"),
      );
    }

    try {
      stream?.write(stringLog + "\n");
    } catch (error) {
      // Silently fail to prevent 500 errors in runtime
    }
  }

  async end() {
    if (!this.enabled) {
      return;
    }

    this.enabled = false;

    await Promise.all(
      [this.streamLogs, this.streamErrors]
        .filter((stream) => stream !== null)
        .map(
          (stream) =>
            new Promise((res) => {
              stream.on("close", res);
              stream.end();
            }),
        ),
    );
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
