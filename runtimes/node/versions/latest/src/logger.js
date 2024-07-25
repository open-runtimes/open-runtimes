const fs = require('fs');

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

        if (this.enabled) {
            this.id = id
                ? id
                : process.env.OPEN_RUNTIMES_ENV === 'development'
                  ? 'dev'
                  : this.generateId();
            this.streamLogs = fs.createWriteStream(
                `/mnt/logs/${this.id}_logs.log`,
                {
                    flags: 'a',
                }
            );
            this.streamErrors = fs.createWriteStream(
                `/mnt/logs/${this.id}_errors.log`,
                {
                    flags: 'a',
                }
            );
        }
    }

    write(message, type = Logger.TYPE_LOG, native = false) {
        if (!this.enabled) {
            return;
        }

        if (native && !this.includesNativeInfo) {
            this.includesNativeInfo = true;
            this.write(
                'Native logs detected. Use context.log() or context.error() for better experience.'
            );
        }

        const stream =
            type === Logger.TYPE_ERROR ? this.streamErrors : this.streamLogs;

        let stringLog = '';
        if (message instanceof Error) {
            stringLog = [message.stack || message].join('\n');
        } else if (message instanceof Object || Array.isArray(message)) {
            stringLog = JSON.stringify(message);
        } else {
            stringLog = `${message}`;
        }

        stream.write(stringLog + '\n');
    }

    async end() {
        if (!this.enabled) {
            return;
        }

        this.enabled = false;

        await Promise.all([
            new Promise((res) => {
                this.streamLogs.end(undefined, undefined, res);
            }),
            new Promise((res) => {
                this.streamErrors.end(undefined, undefined, res);
            }),
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
                (...args) => {
                    const formattedArgs = args.map((arg) =>
                        typeof arg === 'object' ? JSON.stringify(arg) : arg
                    );
                    this.write(formattedArgs.join(' '), Logger.TYPE_LOG, true);
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
        const baseId = sec.toString(16) + msec.toString(16).padStart(5, '0');
        let randomPadding = '';
        for (let i = 0; i < padding; i++) {
            const randomHexDigit = Math.floor(Math.random() * 16).toString(16);
            randomPadding += randomHexDigit;
        }
        return baseId + randomPadding;
    }
}

module.exports = Logger;
