const fs = require("fs");
const micro = require("micro");
const { text: parseText, json: parseJson, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    const timeout = req.headers[`x-open-runtimes-timeout`] ?? '';
    let safeTimeout = null;
    if(timeout) {
        if(isNaN(timeout)) {
            return send(res, 500, 'Header "x-open-runtimes-timeout" must be an integer.');
        }
        
        safeTimeout = +timeout;
    }

    if (!req.headers[`x-open-runtimes-secret`] || req.headers[`x-open-runtimes-secret`] !== (process.env['OPEN_RUNTIMES_SECRET'] ?? '')) {
        safeTimeout && clearTimeout(safeTimeout);
        return send(res, 500, 'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    }

    const logs = [];
    const errors = [];

    const contentType = req.headers['content-type'] ?? 'text/plain';
    const bodyString = await parseText(req);
    let body = bodyString;

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
            bodyString,
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
                return {
                    body: body,
                    statusCode: statusCode,
                    headers: headers
                }
            },
            json: function (obj, statusCode = 200, headers = {}) {
                headers['content-type'] = 'application/json';
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
    try {
        let userFunction = require(USER_CODE_PATH + '/' + process.env.OPEN_RUNTIMES_ENTRYPOINT);

        if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
            throw new Error("User function is not valid.");
        }

        let fx;
        if (userFunction.default) {
            if (!(userFunction.default.constructor || userFunction.default.call || userFunction.default.apply)) {
                throw new Error("User function is not valid.");
            }

            fx = userFunction.default(context);
        } else {
            fx = userFunction(context);
        }

        if(safeTimeout) {
            output = await Promise.race([fx, new Promise((promiseRes, promiseRej) => {
                setTimeout(() => {
                    promiseRej('Execution timed out.');
                }, safeTimeout * 1000);
            })]);
        } else {
            output = await fx;
        }

        output = await fx;
    } catch (e) {
        context.error(e.code === 'MODULE_NOT_FOUND' ? "Code file not found." : e.stack || e);
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

    for (const header in output.headers) {
        if(header.toLowerCase().startsWith('x-open-runtimes-')) {
            continue;
        }
        
        res.setHeader(header.toLowerCase(), output.headers[header]);
    }

    if(customstd) {
        context.log('Unsupported log noticed. Use context.log() or context.error() for logging.');
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));

    safeTimeout && clearTimeout(safeTimeout);

    return send(res, output.statusCode, output.body);
});

server.listen(3000);