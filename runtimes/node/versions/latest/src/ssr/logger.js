import { createWriteStream, writeFileSync, unlinkSync } from "fs";

const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static streams = [];

  static nativeLog(message) {
    nativeLog(message);
  }

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

    if (Logger.streams[id]) {
      return id;
    }

    Logger.streams[id] = {
      logs: createWriteStream(`/mnt/logs/${id}_logs.log`, {
        flags: "a",
      }),
      errors: createWriteStream(`/mnt/logs/${id}_errors.log`, {
        flags: "a",
      }),
    };

    try {
      writeFileSync(`/mnt/logs/${id}_logs.log.lock`, "");
      writeFileSync(`/mnt/logs/${id}_errors.log.lock`, "");
    } catch (err) {
      // Cuncurrent dev request, not a big deal
    }

    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG) {
    const streams = Logger.streams[id];
    if (!streams) {
      return;
    }

    const stream = type === Logger.TYPE_ERROR ? streams.errors : streams.logs;

    let stringLog = "";
    for (let i = 0; i < messages.length; i++) {
      const message = messages[i];
      if (message instanceof Error) {
        stringLog += [message.stack || message].join("\n");
      } else if (message instanceof Object || Array.isArray(message)) {
        stringLog += JSON.stringify(message);
      } else {
        stringLog += `${message}`;
      }

      if (i < messages.length - 1) {
        stringLog += " ";
      }
    }

    if (stream.writable) {
      stream.write(stringLog + "\n");
    } else {
      nativeLog(stringLog + "\n");
    }
  }

  static async end(id) {
    const streams = Logger.streams[id];
    if (!streams) {
      return;
    }

    await Promise.all([
      new Promise((res) => {
        streams.logs.end(undefined, undefined, res);
      }),
      new Promise((res) => {
        streams.errors.end(undefined, undefined, res);
      }),
    ]);

    try {
      unlinkSync(`/mnt/logs/${id}_logs.log.lock`);
      unlinkSync(`/mnt/logs/${id}_errors.log.lock`);
    } catch (err) {
      // Cuncurrent dev request, not a big deal
    }

    delete Logger.streams[id];
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
