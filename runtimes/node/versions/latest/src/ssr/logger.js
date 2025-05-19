import { promises as fs } from "fs";

export const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static buffers = new Map();
  static #savedConsole = null;

  constructor(status, id) {
    this.id = Logger.start(status, id);
  }

  write(messages, type = Logger.TYPE_LOG) {
    Logger.write(this.id, messages, type);
  }

  end() {
    return Logger.end(this.id);
  }

  overrideNativeLogs() {
    Logger.overrideNativeLogs(this.id);
  }

  revertNativeLogs() {
    Logger.revertNativeLogs();
  }

  static start(status, id) {
    if ((status ?? "enabled") !== "enabled") return "";
    if (!id)
      id =
        process.env.OPEN_RUNTIMES_ENV === "development"
          ? "dev"
          : Logger.#generateId();
    if (!Logger.buffers.has(id))
      Logger.buffers.set(id, { logs: [], errors: [] });
    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG) {
    const entry = Logger.buffers.get(id);
    if (!entry) return;
    const line = messages
      .map((m) =>
        m instanceof Error
          ? (m.stack ?? m).toString()
          : typeof m === "object"
            ? JSON.stringify(m)
            : String(m),
      )
      .join(" ");
    (type === Logger.TYPE_ERROR ? entry.errors : entry.logs).push(line);
  }

  static async end(id) {
    const entry = Logger.buffers.get(id);
    if (!entry) return;
    await Promise.all([
      Logger.#flush(
        id,
        entry.logs.join("\n") + (entry.logs.length ? "\n" : ""),
        "logs",
      ),
      Logger.#flush(
        id,
        entry.errors.join("\n") + (entry.errors.length ? "\n" : ""),
        "errors",
      ),
    ]);
    Logger.buffers.delete(id);
  }

  static overrideNativeLogs(id) {
    if (Logger.#savedConsole) return;
    Logger.#savedConsole = { ...console };
    const proxy =
      (type) =>
      (...args) => {
        Logger.write(id, args, type);
        Logger.write(
          id,
          [
            "Native logs detected. Use context.log() or context.error() for better experience.",
          ],
          type,
        );
      };
    console.log =
      console.info =
      console.debug =
      console.warn =
        proxy(Logger.TYPE_LOG);
    console.error = proxy(Logger.TYPE_ERROR);
  }

  static revertNativeLogs() {
    if (!Logger.#savedConsole) return;
    Object.assign(console, Logger.#savedConsole);
    Logger.#savedConsole = null;
  }

  static async #flush(id, data, suffix) {
    if (!data.trim()) return;
    const tmp = `/mnt/logs/${id}_${suffix}.tmp`;
    const final = `/mnt/logs/${id}_${suffix}.log`;
    const fd = await fs.open(tmp, "w");
    try {
      await fd.writeFile(data, "utf8");
      await fd.sync();
    } finally {
      await fd.close();
    }
    await fs.rename(tmp, final);
  }

  // Recreated from https://www.php.net/manual/en/function.uniqid.php
  static #generateId(padding = 7) {
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
