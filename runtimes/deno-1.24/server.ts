import { Application } from "https://deno.land/x/oak@v10.6.0/mod.ts";

const USER_CODE_PATH = '/usr/code-start';

const app = new Application();

app.use(async (ctx) => {
  if (ctx.request.headers.get("x-open-runtimes-secret") !== Deno.env.get("OPEN_RUNTIMES_SECRET")) {
    ctx.response.status = 500;
    ctx.response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    return;
  }
  const logs: string[] = [];
  const errors: string[] = [];

  const contentType = ctx.request.headers.get('content-type') ?? 'text/plain';
  const rawBody: string = await ctx.request.body({ type: 'text' }).value;
  let body: any = rawBody;

  if (contentType.includes('application/json')) {
    body = await ctx.request.body({ type: 'json' }).value;
  }

  const headers: any = {};
  Array.from(ctx.request.headers.keys()).filter((header: any) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header: any) => {
    headers[header.toLowerCase()] = ctx.request.headers.get(header);
  });

  let url = ctx.request.url.pathname;

  if(ctx.request.url.href.includes('?')) {
    url += `?${ctx.request.url.href.split('?')[1]}`;
  }

  const context: any = {
    req: {
      rawBody,
      body,
      headers,
      method: ctx.request.method,
      url
    },
    res: {
      send: function (body, statusCode = 200, headers = {}) {
        return {
          body: body,
          statusCode: statusCode,
          headers: headers
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
    log: function () {
      const args: string[] = [];
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
      const args: string[] = [];
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

  let output: any = null;
  try {
    const userFunction = (await import(USER_CODE_PATH + '/' + Deno.env.get("OPEN_RUNTIMES_ENTRYPOINT"))).default;

    if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
      throw new Error("User function is not valid.");
    }

    output = await userFunction(context);
  } catch(e: any) {
    context.error(e.message.includes("Cannot resolve module") ? "Code file not found." : e.stack || e);
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
    
    ctx.response.headers.set(header.toLowerCase(), output.headers[header]);
  }

  ctx.response.headers.set('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
  ctx.response.headers.set('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));	

  ctx.response.status = output.statusCode;
  if(output.statusCode !== 204) {
    ctx.response.body = output.body;
  }
});

await app.listen({ port: 3000 });