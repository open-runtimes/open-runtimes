const USER_CODE_PATH = '/usr/code-start';

function sendJson(body: any, statusCode: number = 200) {
    return sendRaw(JSON.stringify(body), statusCode, {
        'Content-Type': 'application/json',
    });
}

function sendRaw(body: any, statusCode: number = 200, headers: any = {}) {
    return new Response(body, {
        status: statusCode,
        headers
    });
}


async function server(req: Request) {
    let body: any = {};
    try {
        body = await req.json();
    } catch (e) {
        return sendJson({stderr: 'Invalid JSON body'}, 500);
    }

    if (req.headers.get('x-internal-challenge') !== process.env['INTERNAL_RUNTIME_KEY']) {
        return sendJson({stderr: 'Unauthorized', headers: req.headers}, 500);
    }

    const request = {
        headers  : Object.fromEntries(req.headers),
        variables: body.variables ?? {},
        payload  : body.payload ?? '',
    };

    const tmpConsole: Record<any, any> = {};

    tmpConsole.stdlog   = console.log.bind(console);
    tmpConsole.stderror = console.error.bind(console);
    tmpConsole.stdinfo  = console.info.bind(console);
    tmpConsole.stddebug = console.debug.bind(console);
    tmpConsole.stdwarn  = console.warn.bind(console);

    const logs: any[]   = [];
    const errors: any[] = [];

    console.log = console.info = console.debug = function () {
        const args: string[] = [];
        Array.from(arguments).forEach(arg => {
            if (arg instanceof Object || Array.isArray(arg)) {
                args.push(JSON.stringify(arg));
            } else {
                args.push(arg)
            }
        });
        logs.push(args.join(" "));
    }

    console.error = console.warn = function () {
        const args: string[] = [];
        Array.from(arguments).forEach(arg => {
            if (arg instanceof Object || Array.isArray(arg)) {
                args.push(JSON.stringify(arg));
            } else {
                args.push(arg)
            }
        });
        errors.push(args.join(" "));
    }

    const response = {
        send: (text: string, status = 200) => sendRaw(text, status),
        json: (json: any, status = 200) => sendJson({response: json, stdout: logs.join('\n'), stderr: errors.join('\n')}, status),
    };

    try {
        let userFunction = require(USER_CODE_PATH + '/' + process.env.INTERNAL_RUNTIME_ENTRYPOINT);
        if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
            throw new Error("User function is not valid.")
        }

        if (userFunction.default) {
            if (!(userFunction.default.constructor || userFunction.default.call || userFunction.default.apply)) {
                throw new Error("User function is not valid.")
            }

            return await userFunction.default(request, response);
        } else {
            return await userFunction(request, response);
        }
    } catch (e: any) {
        return sendJson({stdout: logs.join('\n'), stderr: errors.join('\n') + "\n" + e.code === 'MODULE_NOT_FOUND' ? "Code file not found." : e.stack || e}, 500);
    }


    console.log   = tmpConsole.stdlog;
    console.error = tmpConsole.stderror;
    console.debug = tmpConsole.stddebug;
    console.warn  = tmpConsole.stdwarn;
    console.info  = tmpConsole.stdinfo;

}

Bun.serve({
    port: 3000,
    fetch(req) {
        return server(req);
    },
});
