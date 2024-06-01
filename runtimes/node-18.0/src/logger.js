
const util = require("node:util");
const fs = require("node:fs");

class Logger {
    static TYPE_ERROR = 'error';
    static TYPE_LOG = 'log';

    id = '';
    enabled = false;
    includesNativeInfo = false;

    streamLogs = null;
    streamErrors = null;
    nativeLogsCache = {};

    constructor(status, id) {
        this.enabled = (status ? status : 'enabled') === 'enabled';

        if(this.enabled) {
            this.id = id ? id : this.generateId();
            this.streamLogs = fs.createWriteStream(`/mnt/logs/${this.id}_logs.log`);
            this.streamErrors = fs.createWriteStream(`/mnt/logs/${this.id}_errors.log`);
        }
    }

    generateId() {
        return `${new Date().getTime().toString(16)}${Math.round(Math.random() * 1000000000).toString(16)}`;
    }

    write(message, type = Logger.TYPE_LOG, native = false) {
        if(!this.enabled) {
            return;
        }

        if(native && !this.includesNativeInfo) {
            this.includesNativeInfo = true;
            this.write('Native logs detected. Use context.log() or context.error() for better experience.');
        }

        const stream = type === Logger.TYPE_ERROR ? this.streamErrors : this.streamLogs;

        let stringLog = "";
        if(message instanceof Error) {
            stringLog = [message.stack || message].join('\n');;
        } else if (message instanceof Object || Array.isArray(message)) {
            stringLog = JSON.stringify(message);
        } else {
            stringLog = `${message}`;
        }

        stream.write(stringLog + '\n');
    }

    async end() {
        if(!this.enabled) {
            return;
        }

        await Promise.all([
            new Promise((res) => {
                this.streamLogs.end(undefined, undefined, res);
            }),
            new Promise((res) => {
                this.streamErrors.end(undefined, undefined, res);
            })
        ]);

        this.enabled = false;
    }

    overrideNativeLogs() {
        if(!this.enabled) {
            return;
        }

        this.nativeLogsCache.stdlog = console.log.bind(console);
        this.nativeLogsCache.stderror = console.error.bind(console);
        this.nativeLogsCache.stdinfo = console.info.bind(console);
        this.nativeLogsCache.stddebug = console.debug.bind(console);
        this.nativeLogsCache.stdwarn = console.warn.bind(console);

        console.log = console.info = console.debug = console.warn = console.error = () => {
            this.write(util.format.apply(null, arguments) + '\n', Logger.TYPE_LOG, true);
        }
    }

    revertNativeLogs() {
        if(!this.enabled) {
            return;
        }

        console.log = this.nativeLogsCache.stdlog;
        console.error = this.nativeLogsCache.stderror;
        console.debug = this.nativeLogsCache.stddebug;
        console.warn = this.nativeLogsCache.stdwarn;
        console.info = this.nativeLogsCache.stdinfo;
    }
}

module.exports = Logger;