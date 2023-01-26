const fs = require("fs");
const micro = require("micro");
const { text: parseText, json: parseJson, buffer: parseBuffer, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    if (req.headers[`x-open-runtimes-secret`] !== process.env['OPEN_RUNTIMES_SECRET']) {
        return send(res, 500, 'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    }

    const logs = [];
    const errors = [];
    const response = {
        headers: {},
        body: '',
        statusCode: 200,
    };

    const contentType = req.headers['content-type'] ?? 'text/plain';
    const rawBody = await parseText(req);
    let body = rawBody;

    if (contentType.includes('application/json')) {
        body = await parseJson(req);
    }

    const headers = {};
    Object.keys(req.headers).filter((header) => !header.startsWith('x-open-runtimes-')).forEach((header) => {
        headers[header] = req.headers[header];
    });

    const context = {
        req: {
            rawBody,
            body,
            headers,
            method: req.method,
            url: req.url
        },
        res: {
            send: function (body, statusCode = 200, headers = {}) {
                if (body !== undefined) { response.body = body; }
                if (statusCode !== undefined) { response.statusCode = statusCode; }
                if (headers !== undefined) { response.headers = headers; }

                return {
                    body: response.body,
                    statusCode: response.statusCode,
                    headers: response.headers
                }
            },
            json: function (obj, statusCode = 200, headers = {}) {
                headers['Content-Type'] = 'application/json';
                return this.send(JSON.stringify(obj), statusCode, headers);
            },
            empty: function () {
                return this.send('', 204, {});
            },
            redirect: function (url, statusCode = 301, headers = {}) {
                headers['Location'] = url;
                return this.send('', statusCode, headers);
            }
        },
        log: function () {
            const args = [];
            for (const arg of Array.from(arguments)) {
                if (arg instanceof Object || Array.isArray(arg)) {
                    args.push(JSON.stringify(arg));
                } else {
                    args.push(arg);
                }
            }
            logs.push(args.join(" "));
        },
        error: function () {
            const args = [];
            for (const arg of Array.from(arguments)) {
                if (arg instanceof Object || Array.isArray(arg)) {
                    args.push(JSON.stringify(arg));
                } else {
                    args.push(arg);
                }
            }
            errors.push(args.join(" "));
        },
    };

    console.log = console.info = console.debug = console.warn = console.error = function() {
        logs.push('Unsupported log noticed. Use context.log() or context.error() for logging.');
    }

    let output = null;
    try {
        let userFunction = require(USER_CODE_PATH + '/' + process.env.OPEN_RUNTIMES_ENTRYPOINT);

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
    } catch (e) {
        context.error(e.code === 'MODULE_NOT_FOUND' ? "Code file not found." : e.stack || e);
        output = context.res.send('', 500, {});
    }

    if(output === null || output === undefined) {
        context.error('Return statement missing. return context.res.empty() if no response is expected.');
        output = context.res.send('', 500, {});
    }

    output.body = output.body ?? '';
    output.statusCode = output.statusCode ?? 200;
    output.headers = output.headers ?? {};

    for (const header in output.headers) {
        if(header.startsWith('x-open-runtimes-')) {
            continue;
        }
        
        res.setHeader(header, output.headers[header]);
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));

    send(res, output.statusCode, output.body);
});

server.listen(3000);