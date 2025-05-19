import { promises as fs } from 'fs';

export const nativeLog = console.log.bind(console);

export class Logger {
  static TYPE_ERROR = 'error';
  static TYPE_LOG = 'log';

  /**
   * Per‑request string buffers → { logs: string[], errors: string[] }
   * Keyed by request‑unique id.
   */
  static buffers = new Map();

  static #savedConsole = null;

  constructor(status, id) {
    this.id = Logger.start(status, id);
  }

  write(messages, type = Logger.TYPE_LOG) {
    Logger.write(this.id, messages, type);
  }

  async end() {
    await Logger.end(this.id);
  }

  overrideNativeLogs(namespace = null) {
    Logger.overrideNativeLogs(namespace);
  }

  revertNativeLogs() {
    Logger.revertNativeLogs();
  }

  static start(status, id) {
    const enabled = (status ? status : 'enabled') === 'enabled';
    if (!enabled) return '';

    if (!id) {
      id = process.env.OPEN_RUNTIMES_ENV === 'development' ? 'dev' : Logger.#generateId();
    }

    if (!Logger.buffers.has(id)) {
      Logger.buffers.set(id, { logs: [], errors: [] });
    }

    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG) {
    const entry = Logger.buffers.get(id);
    if (!entry) return;

    let stringLog = '';
    for (let i = 0; i < messages.length; i++) {
      const message = messages[i];
      if (message instanceof Error) {
        stringLog += (message.stack || message).toString();
      } else if (typeof message === 'object') {
        stringLog += JSON.stringify(message);
      } else {
        stringLog += `${message}`;
      }
      if (i < messages.length - 1) stringLog += ' ';
    }

    (type === Logger.TYPE_ERROR ? entry.errors : entry.logs).push(stringLog);
  }

  static async end(id) {
    const entry = Logger.buffers.get(id);
    if (!entry) return;

    await Promise.all([
      Logger.#flush(id, entry.logs.join('\n') + (entry.logs.length ? '\n' : ''), 'logs'),
      Logger.#flush(id, entry.errors.join('\n') + (entry.errors.length ? '\n' : ''), 'errors')
    ]);

    Logger.buffers.delete(id);
  }

  static overrideNativeLogs(namespace = null) {
    if (Logger.#savedConsole) return; // already overridden

    Logger.#savedConsole = {
      log: console.log,
      info: console.info,
      debug: console.debug,
      warn: console.warn,
      error: console.error
    };

    const makeProxy = (type) =>
      (...args) => {
        const requestId = namespace && typeof namespace.get === 'function' ? namespace.get('id') : undefined;
        // Fallback to dev when no async‑local id available.
        const id = requestId || 'dev';
        Logger.write(id, args, type);
      };

    console.log = console.info = console.debug = console.warn = makeProxy(Logger.TYPE_LOG);
    console.error = makeProxy(Logger.TYPE_ERROR);
  }

  static revertNativeLogs() {
    if (!Logger.#savedConsole) return;

    console.log = Logger.#savedConsole.log;
    console.info = Logger.#savedConsole.info;
    console.debug = Logger.#savedConsole.debug;
    console.warn = Logger.#savedConsole.warn;
    console.error = Logger.#savedConsole.error;

    Logger.#savedConsole = null;
  }

  static async #flush(id, data, suffix) {
    if (!data.trim()) return; // skip empty files

    const tmpPath = `/mnt/logs/${id}_${suffix}.tmp`;
    const finalPath = `/mnt/logs/${id}_${suffix}.log`;

    const fd = await fs.open(tmpPath, 'w');
    try {
      await fd.writeFile(data, 'utf8');
      await fd.sync(); // ensure data reached NFS server
    } finally {
      await fd.close();
    }

    await fs.rename(tmpPath, finalPath); // atomic publish
  }

  static #generateId(padding = 7) {
    const now = new Date();
    const sec = Math.floor(now.getTime() / 1000).toString(16);
    const msec = now.getMilliseconds().toString(16).padStart(5, '0');
    let random = '';
    for (let i = 0; i < padding; i++) random += Math.floor(Math.random() * 16).toString(16);
    return sec + msec + random;
  }
}
