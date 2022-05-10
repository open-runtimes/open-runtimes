import { Application } from "https://deno.land/x/oak@v10.5.1/mod.ts";
import * as path from "https://deno.land/std@0.138.0/path/mod.ts";

const USER_CODE_PATH = '/usr/code-start';

const app = new Application();

app.use(async (ctx) => {
  const { value } = ctx.request.body({ type: 'json' });
  const body = await value;

  if (ctx.request.headers.get("x-internal-challenge") !== Deno.env.get("INTERNAL_RUNTIME_KEY")) {
    ctx.response.status = 500;
    ctx.response.body = {stderr: "Unauthorized"};
    return;
  }

  const request = {
    env: body.env ?? {},
    headers: body.headers ?? {},
    payload: body.payload ?? ''
  };

  const stdlog = console.log.bind(console);
  const stderror = console.error.bind(console);
  const stdinfo = console.info.bind(console);
  const stddebug = console.debug.bind(console);
  const stdwarn = console.warn.bind(console);
  let logs = [];

  console.log = console.error = console.info = console.warn = console.debug = function(){
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

  const response = {
    send: (text: string, status = 200) => {
      ctx.response.status = status;
      ctx.response.body = {response: text, stdout: logs.join('\n')};
    },
    json: (json: Record<string, unknown>, status = 200) => {
      ctx.response.status = status;
      ctx.response.body = {response: json, stdout: logs.join('\n')};
    }
  };

  try {
    const userFunction = (await import(USER_CODE_PATH + '/' + Deno.env.get("INTERNAL_RUNTIME_ENTRYPOINT"))).default;

    if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
      throw new Error("User function is not valid.")
    }

    await userFunction(request, response);
  } catch (error) {
    ctx.response.status = 500;
    ctx.response.body = {stdout: logs.join('\n'), stderr: error.message.includes("Cannot resolve module") ? 'Code file not found.' : error.stack || error.message};
  }
  logs = [];
  console.log = stdlog;
  console.error = stderror;
  console.debug = stddebug;
  console.warn = stdwarn;
  console.info = stdinfo;
});

await app.listen({ port: 3000 });