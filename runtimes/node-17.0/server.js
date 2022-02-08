const path = require("path");
const micro = require("micro");
const { json, send } = require("micro");

const server = micro(async (req, res) => {
    const body = await json(req);

    if (req.headers['x-internal-challenge'] !== process.env['INTERNAL_RUNTIME_KEY']) {
        return send(res, 401, { code: 401, message: 'Unauthorized' });
    }

    const request = {
        env: body.env ?? {},
        headers: body.headers ?? {},
        payload: body.payload ?? {},
    };

    let isResponded = false;
    const response = {
        send: (text, status = 200) => {
            isResponded = true;
            send(res, status, text);
        },
        json: (json, status = 200) => {
            isResponded = true;
            send(res, status, json);
        },
    };

    try {
        let userFunction = require(path.join(body.path ?? '/usr/code', body.file ?? 'index.js'));

        if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
            throw new Error('Function not found.');
        }

        await userFunction(request, response);

        if(!isResponded) {
            send(res, 200, {
                code: 200,
                message: 'No code output.'
            });
        }
    } catch (e) {
        send(res, 500, {
            code: 500,
            message: e.code === 'MODULE_NOT_FOUND' ? 'Code file not found.' : e.stack || e
        });
    }
});

server.listen(3000);
