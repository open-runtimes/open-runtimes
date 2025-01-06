import {
  readFileSync,
  existsSync,
  createWriteStream,
  writeFileSync,
  unlinkSync,
} from "fs";

// TODO: Temporary
const stdlog = (log) => {
  let d = "";
  if (existsSync("/mnt/logs/local.txt")) {
    d = readFileSync("/mnt/logs/local.txt").toString();
  }
  writeFileSync("/mnt/logs/local.txt", d + "\n" + log);
};

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static streams = [];

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

    if (Logger.stream[id]) {
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

    stream.write(stringLog + "\n");
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

  static overrideNativeLogs(namespace) {
    console.log =
      console.info =
      console.debug =
      console.warn =
        (...args) => {
          const requestId = namespace.get("id");
          stdlog(requestId);
          Logger.write(requestId, args, Logger.TYPE_LOG, true);
        };

    console.error = (...args) => {
      const requestId = namespace.get("id");
      stdlog(requestId);
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
