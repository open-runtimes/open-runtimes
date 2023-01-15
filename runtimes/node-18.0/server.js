const fs = require("fs");
const micro = require("micro");
const { parse: parseUrlEncoded } = require("qs");
const { parse: parseMultipart } = require('parse-multipart-data');
const { text: parseText, json: parseJson, buffer: parseBuffer, send } = require("micro");

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
        statusCode: 200,

        cookies: {},
        contentType: null,
    };

    const contentType = req.headers['content-type'] ?? 'text/plain';
    let body = null;

    if (contentType.includes('application/json')) {
        body = await parseJson(req);
    } else if (contentType.includes('application/x-www-form-urlencoded')) {
        body = parseUrlEncoded(await parseText(req));
    } else if (contentType.includes('multipart/form-data')) {
        const boundarySearch = contentType.match(/boundary=(.*)/);
        if (boundarySearch) {
            const boundary = boundarySearch[1];
            body = parseMultipart((await parseBuffer(req)), boundary); // TODO: Always gives empty array, making test fail
        }
    }

    if (body === null) {
        body = await parseText(req);
    }

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
            getBody: function () {
                return response.body;
            },

            getStatusCode: function () {
                return response.statusCode;
            },

            getContentType: function () {
                return response.contentType
            },

            getCookies: function () {
                return response.cookies;
            },

            getHeaders: function () {
                return response.headers;
            },

            getHeader: function (key) {
                return response.headers[key] ?? null;
            },

            setBody: function (body) {
                response.body = body;
                return this;
            },

            setStatusCode: function (statusCode) {
                response.statusCode = statusCode;
                return this;
            },

            setContentType: function (contentType) {
                response.contentType = contentType;
                return this;
            },

            setCookies: function (cookies) {
                response.cookies = cookies;
                return this;
            },

            setHeaders: function (headers) {
                response.headers = headers;
                return this;
            },

            addHeader: function (key, value) {
                response.headers[key] = value;
                return this;
            },

            addCookie: function (name, value, expire, maxage, path, domain, secure, httponly, samesite) {
                response.cookies[name] = { value, expire, maxage, path, domain, secure, httponly, samesite };
            },

            send: function (body, statusCode, headers) {
                if (body !== undefined) { response.body = body; }
                if (statusCode !== undefined) { response.statusCode = statusCode; }
                if (headers !== undefined) { response.headers = headers; }

                const resHeaders = { ...response.headers };

                if (response.contentType) {
                    resHeaders['content-type'] = response.contentType;
                }

                if (response.cookies && Object.keys(response.cookies).length > 0) {
                    resHeaders['set-cookie'] = Object.keys(response.cookies).map((name) => {
                        const cookie = response.cookies[name];

                        const attributes = [];
                        attributes.push(`${name}=${cookie.value}`);

                        if (cookie.domain) {
                            attributes.push(`Domain=${cookie.domain}`);
                        }
                        if (cookie.expire) {
                            attributes.push(`Expire=${cookie.expire}`);
                        }
                        if (cookie.maxage) {
                            attributes.push(`Max-Age=${cookie.maxage}`);
                        }
                        if (cookie.path) {
                            attributes.push(`Path=${cookie.path}`);
                        }
                        if (cookie.samesite) {
                            attributes.push(`SameSite=${cookie.samesite}`);
                        }
                        if (cookie.secure) {
                            attributes.push(`Secure`);
                        }
                        if (cookie.httponly) {
                            attributes.push(`HttpOnly`);
                        }

                        return attributes.join('; ');
                    }).join('; ');
                }

                return {
                    body: response.body,
                    statusCode: response.statusCode,
                    headers: resHeaders
                }
            },
            empty: function () {
                return this.send('', 204, {});
            },
            json: function (obj, statusCode, headers = {}) {
                headers['content-type'] = 'application/json';
                return this.send(JSON.stringify(obj), statusCode, headers);
            },
            html: function (html, statusCode, headers = {}) {
                headers['content-type'] = 'text/html';
                return this.send(html, statusCode, headers);
            },
            file: function (path, statusCode, headers = {}) {
                return this.send(fs.readFileSync(path), statusCode, headers);
            },
            redirect: function (url, statusCode = 301, headers = {}) {
                headers['location'] = url;
                return this.send(undefined, statusCode, headers);
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

    if (!output) {
        output = {};
    }

    output.body = output.body ?? '';
    output.statusCode = output.statusCode ?? 204;
    output.headers = output.headers ?? {};

    for (const header in output.headers) {
        res.setHeader(header, output.headers[header]);
    }

    res.setHeader('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));

    send(res, output.statusCode, output.body);
});

server.listen(3000);