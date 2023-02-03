const fs = require("fs");
const micro = require("micro");
const { text: parseText, json: parseJson, buffer: parseBuffer, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    if ((req.headers[`x-open-runtimes-secret`] ?? '') !== process.env['OPEN_RUNTIMES_SECRET']) {
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
    const bodyString = await parseText(req);
    let body = bodyString;

    if (contentType.includes('application/json')) {
        if(bodyString) {
            body = await parseJson(req);
        } else {
            body = {};
        }
    }

    const headers = {};
    Object.keys(req.headers).filter((header) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header) => {
        headers[header.toLowerCase()] = req.headers[header];
    });

    const scheme = (req.headers['x-forwarded-proto'] ?? '') === 'https' ? 'https' : 'http';
    const host = req.headers['host'].includes(':') ? req.headers['host'].split(':')[0] : req.headers['host'];
    const port = +(req.headers['host'].includes(':') ? req.headers['host'].split(':')[1] : '80');
    const path = req.url.includes('?') ? req.url.split('?')[0] : req.url;
    const queryString = req.url.includes('?') ? req.url.split('?')[1] : '';
    const query = {};
    for(const param of queryString.split('&')) {
        const [ key, value ] = param.split('=');

        if(key) {
            query[key] = value;
        }
    }

    const url = `${scheme}://${host}${port === 80 ? '' : `:${port}`}${path}${queryString === '' ? '' : `?${queryString}`}`;

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
        if(header.toLowerCase().startsWith('x-open-runtimes-')) {
            continue;
        }
        
        res.setHeader(header.toLowerCase(), output.headers[header]);
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));

    send(res, output.statusCode, output.body);
});

server.listen(3000);