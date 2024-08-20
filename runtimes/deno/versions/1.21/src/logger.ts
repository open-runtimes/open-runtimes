export class Logger {
  static TYPE_ERROR = "error";
  static TYPE_LOG = "log";

  writePromises: Promise<any>[] = [];

  id = "";
  enabled = false;
  includesNativeInfo = false;

  streamLogs: Deno.FsFile | null = null;
  streamErrors: Deno.FsFile | null = null;
  nativeLogsCache: { [key: string]: any } = {};

  constructor(status: string | null = null, id: string | null = null) {
    this.enabled = (status ? status : "enabled") === "enabled";

    if (this.enabled) {
      this.id = id
        ? id
        : (Deno.env.get("OPEN_RUNTIMES_ENV") === "development"
          ? "dev"
          : this.generateId());
    }
  }

  async setup() {
    if (this.enabled) {
      const [streamLogs, streamErrors] = await Promise.all([
        Deno.open(`/mnt/logs/${this.id}_logs.log`, {
          create: true,
          append: true,
        }),
        Deno.open(`/mnt/logs/${this.id}_errors.log`, {
          create: true,
          append: true,
        }),
      ]);

      /*
            Unsupported yet
            await Promise.all([
                streamLogs.ready,
                streamErrors.ready
            ]);
            */

      this.streamLogs = streamLogs;
      this.streamErrors = streamErrors;
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

    const stream = type === Logger.TYPE_ERROR
      ? this.streamErrors
      : this.streamLogs;

    if (!stream) {
      return;
    }

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

    const encoded = new TextEncoder().encode(stringLog + "\n");
    this.writePromises.push(stream.write(encoded));
  }

  async end() {
    if (!this.enabled || !this.streamLogs || !this.streamErrors) {
      return;
    }

    this.enabled = false;

    for (const promise of this.writePromises) {
      await promise;
    }

    await Promise.all([
      this.streamLogs.close(),
      this.streamErrors.close(),
    ]);
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
