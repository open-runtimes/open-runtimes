const fs = require("fs");
const micro = require("micro");
const { text, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    if (req.headers[`x-internal-challenge`] !== process.env['INTERNAL_RUNTIME_KEY']) {
        return send(res, 500, 'Unauthorized');
    }

    const logs = [];
    const errors = [];

    const context = {
        req: {
            body: await text(req),
            // TODO: Add more
        },
        res: {
            headers: {},
            body: '',
            status: 200,

            send: function (body, status, headers) {
                if (body !== undefined) { this.body = body; }
                if (status !== undefined) { this.status = status; }
                if (headers !== undefined) { this.headers = headers; }

                return {
                    body: this.body,
                    status: this.status,
                    headers: this.headers
                }
            },
            json: function (obj, status, headers = {}) {
                headers['content-type'] = 'application/json';
                return this.send(JSON.stringify(obj), status, headers);
            },
            file: function (path, status, headers = {}) {
                headers['content-type'] = 'application/octet-stream';
                return this.send(fs.readFileSync(path), status, headers);
            },
            redirect: function (url, status = 301, headers = {}) {
                headers['location'] = url;
                return this.send(undefined, status, headers);
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

    let output;
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
    output.status = output.status ?? 200;
    output.headers = output.headers ?? {};

    for(const header in output.headers) {
        res.setHeader(header, output.headers[header]);
    }

    res.setHeader('x-opr-logs', encodeURIComponent(logs.join('\n')));
    res.setHeader('x-opr-errors', encodeURIComponent(errors.join('\n')));

    send(res, output.status ?? 200, output.body ?? ''); 
});

server.listen(3000);