import { promises as fs } from "fs";

export const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  static buffers = new Map();

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

  static overrideNativeLogs(namespace) {
    const proxy =
      (type) =>
      (...args) => {
        const id = namespace.get("id");
        Logger.write(id, args, type);
      };
    console.log =
      console.info =
      console.debug =
      console.warn =
        proxy(Logger.TYPE_LOG);
    console.error = proxy(Logger.TYPE_ERROR);
  }

  static async #flush(id, data, suffix) {
    if (!data.trim()) return;

    const tmpPath = `/mnt/logs/${id}_${suffix}.tmp`;
    const finalPath = `/mnt/logs/${id}_${suffix}.log`;

    await fs.writeFile(tmpPath, data);
    await fs.rename(tmpPath, finalPath);
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
