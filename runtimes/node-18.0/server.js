const fs = require("fs");
const micro = require("micro");
const { text, json, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    if (req.headers[`x-open-runtimes-secret`] !== process.env['INTERNAL_RUNTIME_KEY']) {
        return send(res, 500, 'Unauthorized');
    }

    const logs = [];
    const errors = [];
    const response = {
        headers: {},
        body: '',
        code: 200
    };

    const contentType = req.headers['content-type'] ?? 'text/plain';
    const body = contentType === 'application/json' ? await json(req) : await text(req);

    const headers = {};
    Object.keys(req.headers).filter((header) => !header.startsWith('x-open-runtimes-')).forEach((header) => {
        headers[header] = req.headers[header];
    });

    const context = {
        req: {
            body: body,
            headers: headers,
            method: req.method,
            url: req.url
        },
        res: {
            getBody: function() {
                return response.body;
            },

            getCode: function() {
                return response.code;
            },

            getContentType: function() {
                return response.headers['content-type'] ?? null;
            },

            getCookies: function() {
                return response.headers['cookie'] ?? null;
            },

            getHeaders: function() {
                return response.headers;
            },

            getHeader: function(key) {
                return response.headers[key] ?? null;
            },

            setBody: function(body) {
                response.body = body;
                return this;
            },

            setCode: function(code) {
                response.code = code;
                return this;
            },

            setContentType: function(contentType) {
                response.headers['content-type'] = contentType;
                return this;
            },

            setCookies: function(cookies) {
                response.headers['cookie'] = cookies;
                return this;
            },

            setHeaders: function(headers) {
                response.headers = headers;
                return this;
            },

            addHeader: function(key, value) {
                response.headers[key] = value;
                return this;
            },

            addCookie: function(name, value, expire, path, domain, secure, httponly, samesite) {
                // TODO: Implement and test in complexResponse
            },

            send: function (body, code, headers) {
                if (body !== undefined) { response.body = body; }
                if (code !== undefined) { response.code = code; }
                if (headers !== undefined) { response.headers = headers; }

                return {
                    body: response.body,
                    code: response.code,
                    headers: response.headers
                }
            },
            empty: function () {
                return this.send('', 204, {});
            },
            json: function (obj, code, headers = {}) {
                headers['content-type'] = 'application/json';
                return this.send(JSON.stringify(obj), code, headers);
            },
            file: function (path, code, headers = {}) {
                return this.send(fs.readFileSync(path), code, headers);
            },
            redirect: function (url, code = 301, headers = {}) {
                headers['location'] = url;
                return this.send(undefined, code, headers);
            }
        },
        log: function () {
            const args = [];
            for(const arg of Array.from(arguments)) {
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
            for(const arg of Array.from(arguments)) {
                if (arg instanceof Object || Array.isArray(arg)) {
                    args.push(JSON.stringify(arg));
                } else {
                    args.push(arg);
                }
            }
            errors.push(args.join(" "));
        },
    };

    let output = {};
    try {
        let userFunction = require(USER_CODE_PATH + '/' + process.env.INTERNAL_RUNTIME_ENTRYPOINT);

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
        output = context.res.send('', 500);
    }

    if(!output) {
        output = {};
    }

    output.body = output.body ?? '';
    output.code = output.code ?? 204;
    output.headers = output.headers ?? {};

    for(const header in output.headers) {
        res.setHeader(header, output.headers[header]);
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));
    
    send(res, output.code, output.body); 
});

server.listen(3000);