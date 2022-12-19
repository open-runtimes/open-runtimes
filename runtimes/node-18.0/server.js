const path = require("path");
const micro = require("micro");
const { json, send } = require("micro");

const USER_CODE_PATH = '/usr/code-start';

const server = micro(async (req, res) => {
    const body = await json(req);

    if (req.headers[`x-internal-challenge`] !== process.env['INTERNAL_RUNTIME_KEY']) {
        return send(res, 500, {stderr: 'Unauthorized'});
    }

    const request = {
        variables: body.variables ?? {},
        headers: body.headers ?? {},
        payload: body.payload ?? '',
    };

    console.stdlog = console.log.bind(console);
    console.stderror = console.error.bind(console);
    console.stdinfo = console.info.bind(console);
    console.stddebug = console.debug.bind(console);
    console.stdwarn = console.warn.bind(console);
    logs = [];
    errors = [];
    console.log = console.info = console.debug = function(){
        var args = [];
        Array.from(arguments).forEach(arg => {
            if(arg instanceof Object || Array.isArray(arg)) {    
                args.push(JSON.stringify(arg));
            } else {
                args.push(arg)
            }
        });
        logs.push(args.join(" "));
    }

    console.error = console.warn = function(){
        var args = [];
        Array.from(arguments).forEach(arg => {
            if(arg instanceof Object || Array.isArray(arg)) {    
                args.push(JSON.stringify(arg));
            } else {
                args.push(arg)
            }
        });
        errors.push(args.join(" "));
    }

    let responseSent = false;
    const response = {
        send: (text, status = 200) => responseSent = true && send(res, status, {response: text, stdout: logs.join('\n'), stderr: errors.join('\n')}),
        json: (json, status = 200) => responseSent = true && send(res, status, {response: json, stdout: logs.join('\n'), stderr: errors.join('\n')}),
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
        responseSent = true && send(res, 500, {stdout: logs.join('\n'), stderr: errors.join('\n') + "\n" + e.code === 'MODULE_NOT_FOUND' ? "Code file not found." : e.stack || e});
    } finally {
        if(!responseSent) {
            send(res, 200, {response: 'OK', stdout: logs.join('\n'), stderr: errors.join('\n')});
        }
    }
    logs = [];
    errors = [];
    console.log = console.stdlog;
    console.error = console.stderror;
    console.debug = console.stddebug;
    console.warn = console.stdwarn;
    console.info = console.stdinfo;
});

server.listen(3000);