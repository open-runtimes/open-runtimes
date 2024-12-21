import { createWriteStream } from "fs";

console.log("Open Runtimes logging enabled");

const streamLogs = createWriteStream(`/mnt/logs/ssr_logs.log`, {
  flags: "a",
});

const streamErrors = createWriteStream(`/mnt/logs/ssr_errors.log`, {
  flags: "a",
});

function write(type, messages) {
  const stream = type === "error" ? streamErrors : streamLogs;

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

const nativeLogsCache = {};
nativeLogsCache.stdlog = console.log.bind(console);
nativeLogsCache.stderror = console.error.bind(console);
nativeLogsCache.stdinfo = console.info.bind(console);
nativeLogsCache.stddebug = console.debug.bind(console);
nativeLogsCache.stdwarn = console.warn.bind(console);

console.log = (...args) => {
  write("log", args);
  nativeLogsCache.stdlog(...args);
};
console.info = (...args) => {
  write("log", args);
  nativeLogsCache.stdinfo(...args);
};
console.debug = (...args) => {
  write("log", args);
  nativeLogsCache.stddebug(...args);
};
console.warn = (...args) => {
  write("log", args);
  nativeLogsCache.stdwarn(...args);
};
console.error = (...args) => {
  write("error", args);
  nativeLogsCache.stderror(...args);
};
