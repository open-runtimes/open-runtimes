import pino from "pino";

export const nativeLog = console.log.bind(console);

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

    return id;
  }

  static write(id, messages, type = Logger.TYPE_LOG) {
    if (!Logger.streams[id]) {
      const logsDestination = pino.transport({
        targets: [{
          target: 'pino-pretty',
          options: {
            colorizeObjects: false,
            colorize: false,
            include: '',
            sync: true,
          }
        }, {
          target: 'pino/file',
          options: { destination: `/mnt/logs/${id}_logs.log` }
        }]
      });
      const errorsDestination = pino.transport({
        targets: [{
          target: 'pino-pretty',
          options: {
            colorizeObjects: false,
            colorize: false,
            include: '',
            sync: true,
          }
        }, {
          target: 'pino/file',
          options: { destination: `/mnt/logs/${id}_errors.log` }
        }]
      });
      
      Logger.streams[id] = {
        logsDestination,
        errorsDestination,
        logs: pino(
          {
            timestamp: false,
          },
          logsDestination,
        ),
        errors: pino(
          {
            timestamp: false,
          },
          errorsDestination,
        ),
      };
    }

    const streams = Logger.streams[id];
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

    stream.info("My info");
    stream.info(stringLog);
  }

  static async end(id) {
    const streams = Logger.streams[id];
    if (!streams) {
      return;
    }

    streams.logsDestination.end();
    streams.errorsDestination.end();

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
