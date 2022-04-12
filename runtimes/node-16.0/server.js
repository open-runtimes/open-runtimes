const path = require("path");
const micro = require("micro");
const { json, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    const body = await json(req);

    if (req.headers[`x-internal-challenge`] !== process.env['INTERNAL_RUNTIME_KEY']) {
        return send(res, 500, 'Unauthorized');
    }

    const request = {
        env: body.env ?? {},
        headers: body.headers ?? {},
        payload: body.payload ?? '',
    };

    const response = {
        send: (text, status = 200) => send(res, status, text),
        json: (json, status = 200) => send(res, status, json),
    };
    try {
        let userFunction = require(USER_CODE_PATH + '/' + process.env.INTERNAL_RUNTIME_ENTRYPOINT);

        if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
            throw new Error("User function is not valid.")
        }

        if(userFunction.default) {
            if (!(userFunction.default.constructor || userFunction.default.call || userFunction.default.apply)) {
                throw new Error("User function is not valid.")
            }
            
            await userFunction.default(request, response);
        } else {
            await userFunction(request, response);
        }
    } catch (e) {
        send(res, 500, e.code === 'MODULE_NOT_FOUND' ? "Code file not found." : e.stack || e);
    }
});

server.listen(3000);