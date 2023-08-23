const fs = require("fs");
const micro = require("micro");
const { text: parseText, json: parseJson, send } = require("micro");

const USER_CODE_PATH = '/usr/local/server/src/function';

const server = micro(async (req, res) => {
    const timeout = req.headers[`x-open-runtimes-timeout`] ?? '';
    let safeTimeout = null;
    if(timeout) {
        if(isNaN(timeout) || timeout === 0) {
            return send(res, 500, 'Header "x-open-runtimes-timeout" must be an integer greater than 0.');
        }
        
        safeTimeout = +timeout;
    }

    if (!req.headers[`x-open-runtimes-secret`] || req.headers[`x-open-runtimes-secret`] !== (process.env['OPEN_RUNTIMES_SECRET'] ?? '')) {
        return send(res, 500, 'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    }

    const logs = [];
    const errors = [];

    const contentType = req.headers['content-type'] ?? 'text/plain';
    const bodyRaw = await parseText(req);
    let body = bodyRaw;

    if (contentType.includes('application/json')) {
        body = await parseJson(req);
    }

    const headers = {};
    Object.keys(req.headers).filter((header) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header) => {
        headers[header.toLowerCase()] = req.headers[header];
    });

    const scheme = (req.headers['x-forwarded-proto'] ?? 'http');
    const defaultPort = scheme === 'https' ? '443' : '80';
    const host = req.headers['host'].includes(':') ? req.headers['host'].split(':')[0] : req.headers['host'];
    const port = +(req.headers['host'].includes(':') ? req.headers['host'].split(':')[1] : defaultPort);
    const path = req.url.includes('?') ? req.url.split('?')[0] : req.url;
    const queryString = req.url.includes('?') ? req.url.split('?')[1] : '';
    const query = {};
    for(const param of queryString.split('&')) {
        let [key, ...valueArr] = param.split('=');
        const value = valueArr.join('=');

        if(key) {
            query[key] = value ?? '';
        }
    }

    const url = `${scheme}://${host}${port.toString() === defaultPort ? '' : `:${port}`}${path}${queryString === '' ? '' : `?${queryString}`}`;

    const context = {
        req: {
            bodyRaw,
            body,
            headers,
            method: req.method,
            host,
            scheme,
            query,
            queryString,
            port,
            url,
            path
        },
        res: {
            send: function (body, statusCode = 200, headers = {}) {
                if (!headers['content-type']) {
                    headers['content-type'] = 'text/plain';
                }
                return {
                    body: body,
                    statusCode: statusCode,
                    headers: headers
                }
            },
            json: function (obj, statusCode = 200, headers = {}) {
                if (!headers['content-type']) {
                    headers['content-type'] = 'application/json';
                }
                return this.send(JSON.stringify(obj), statusCode, headers);
            },
            empty: function () {
                return this.send('', 204, {});
            },
            redirect: function (url, statusCode = 301, headers = {}) {
                headers['location'] = url;
                return this.send('', statusCode, headers);
            }
        },
        log: function (message) {
            if (message instanceof Object || Array.isArray(message)) {
                logs.push(JSON.stringify(message));
            } else {
                logs.push(message + "");
            }
        },
        error: function (message) {
            if (message instanceof Object || Array.isArray(message)) {
                errors.push(JSON.stringify(message));
            } else {
                errors.push(message + "");
            }
        },
    };

    console.stdlog = console.log.bind(console);
    console.stderror = console.error.bind(console);
    console.stdinfo = console.info.bind(console);
    console.stddebug = console.debug.bind(console);
    console.stdwarn = console.warn.bind(console);

    let customstd = "";
    console.log = console.info = console.debug = console.warn = console.error = function() {
        customstd += "Native log";
    }

    let output = null;

    async function execute() {
        let userFunction;
        try {
            userFunction = require(USER_CODE_PATH + '/' + process.env.OPEN_RUNTIMES_ENTRYPOINT);
        } catch(err) {
            if(err.code === 'ERR_REQUIRE_ESM') {
                userFunction = await import(USER_CODE_PATH + '/' + process.env.OPEN_RUNTIMES_ENTRYPOINT);
            } else {
                throw err;
            }
        }

        if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
            throw new Error("User function is not valid.");
        }

        if (userFunction.default) {
            if (!(userFunction.default.constructor || userFunction.default.call || userFunction.default.apply)) {
                throw new Error("User function is not valid.");
            }

            output = await userFunction.default(context);
        } else {
            output = await userFunction(context);
        }
    }

    try {
        if(safeTimeout !== null) {
            let executed = true;

            const timeoutPromise = new Promise((promiseRes) => {
                setTimeout(() => {
                    executed = false;
                    promiseRes(true);
                }, safeTimeout * 1000);
            });

            await Promise.race([execute(), timeoutPromise]);

            if(!executed) {
                context.error('Execution timed out.');
                output = context.res.send('', 500, {});
            }
        } else {
            await execute();
        }
    } catch (e) {
        if(e.code === 'MODULE_NOT_FOUND') {
            context.error('Could not load code file.');
        }

        context.error(e.stack || e);
        output = context.res.send('', 500, {});
    } finally {
        console.log = console.stdlog;
        console.error = console.stderror;
        console.debug = console.stddebug;
        console.warn = console.stdwarn;
        console.info = console.stdinfo;
    }

    if(output === null || output === undefined) {
        context.error('Return statement missing. return context.res.empty() if no response is expected.');
        output = context.res.send('', 500, {});
    }

    output.body = output.body ?? '';
    output.statusCode = output.statusCode ?? 200;
    output.headers = output.headers ?? {};

    for (let header in output.headers) {
        header = header.toLowerCase();
        let headerValue = output.headers[header];

        if (header.startsWith("x-open-runtimes-")) {
            continue;
        }

        if (
            header === "content-type" &&
            !headerValue.startsWith("multipart/") &&
            !headerValue.includes("charset=")
        ) {
            headerValue += ";charset=utf-8";
        }

        res.setHeader(header, headerValue);
    }

    if(customstd) {
        context.log('Unsupported log detected. Use context.log() or context.error() for logging.');
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));

    return send(res, output.statusCode, output.body);
});

server.listen(3000);