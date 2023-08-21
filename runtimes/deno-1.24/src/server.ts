import { Application } from "https://deno.land/x/oak@v10.6.0/mod.ts";

const USER_CODE_PATH = '/usr/local/server/src/function';

const app = new Application();

app.use(async (ctx) => {
  const timeout = ctx.request.headers.get(`x-open-runtimes-timeout`) ?? '';
  let safeTimeout: number | null = null;
  if(timeout) {
      if(isNaN(+timeout) || timeout === '0') {
          ctx.response.status = 500;
          ctx.response.body = 'Header "x-open-runtimes-timeout" must be an integer greater than 0.';
          return;
      }

      safeTimeout = +timeout;
  }

  if ((ctx.request.headers.get("x-open-runtimes-secret") ?? '')  === '' || (ctx.request.headers.get("x-open-runtimes-secret") ?? '') !== (Deno.env.get("OPEN_RUNTIMES_SECRET") ?? '')) {
    ctx.response.status = 500;
    ctx.response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    return;
  }
  const logs: string[] = [];
  const errors: string[] = [];

  const contentType = ctx.request.headers.get('content-type') ?? 'text/plain';
  const bodyRaw: string = await ctx.request.body({ type: 'text' }).value;
  let body: any = bodyRaw;

  if (contentType.includes('application/json')) {
    if(bodyRaw) {
      body = await ctx.request.body({ type: 'json' }).value;
    } else {
      body = {};
    }
  }

  const headers: any = {};
  Array.from(ctx.request.headers.keys()).filter((header: any) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header: any) => {
    headers[header.toLowerCase()] = ctx.request.headers.get(header);
  });

  const scheme = ctx.request.headers.get('x-forwarded-proto') ?? 'http';
  const defaultPort = scheme === 'https' ? '443' : '80';
  const hostHeader = ctx.request.headers.get('host') ?? '';
  const host = hostHeader.includes(':') ? hostHeader.split(':')[0] : hostHeader;
  const port = +(hostHeader.includes(':') ? hostHeader.split(':')[1] : defaultPort);
  const path = ctx.request.url.pathname;
  const queryString = ctx.request.url.href.includes('?') ? ctx.request.url.href.split('?')[1] : '';
  const query: any = {};
  for(const param of queryString.split('&')) {
    let [key, ...valueArr] = param.split('=');
    const value = valueArr.join('=');

      if(key) {
          query[key] = value;
      }
  }

  const url = `${scheme}://${host}${port.toString() === defaultPort ? '' : `:${port}`}${path}${queryString === '' ? '' : `?${queryString}`}`;

  const context: any = {
    req: {
      bodyRaw,
      body,
      headers,
      method: ctx.request.method,
      url,
      query,
      queryString,
      host,
      port,
      scheme,
      path
    },
    res: {
      send: function (body: any, statusCode = 200, headers: any = {}) {
        if (!headers['content-type']) {
          headers['content-type'] = 'text/plain';
        }
        return {
          body: body,
          statusCode: statusCode,
          headers: headers
        }
      },
      json: function (obj: any, statusCode = 200, headers: any = {}) {
        if (!headers['content-type']) {
          headers['content-type'] = 'application/json';
        }
        return this.send(JSON.stringify(obj), statusCode, headers);
      },
      empty: function () {
        return this.send('', 204, {});
      },
      redirect: function (url: string, statusCode = 301, headers: any = {}) {
        headers['location'] = url;
        return this.send('', statusCode, headers);
      }
    },
    log: function (message: any) {
      if (message instanceof Object || Array.isArray(message)) {
        logs.push(JSON.stringify(message));
      } else {
        logs.push(message + "");
      }
    },
    error: function (message: any) {
      if (message instanceof Object || Array.isArray(message)) {
        errors.push(JSON.stringify(message));
      } else {
        errors.push(message + "");
      }
    },
  };

  const stdlog = console.log.bind(console);
  const stderror = console.error.bind(console);
  const stdinfo = console.info.bind(console);
  const stddebug = console.debug.bind(console);
  const stdwarn = console.warn.bind(console);

  let customstd = "";
  console.log = console.info = console.debug = console.warn = console.error = function() {
    customstd += "Native log";
  }

  let output: any = null;

  async function execute() {
    const userFunction = (await import(USER_CODE_PATH + '/' + Deno.env.get("OPEN_RUNTIMES_ENTRYPOINT"))).default;

    if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
      throw new Error("User function is not valid.");
    }

    output = await userFunction(context);
  }

  try {
    if(safeTimeout !== null) {
      const safeTimeoutConst: number = safeTimeout;
      let executed = true;

      const timeoutPromise = new Promise((promiseRes) => {
        setTimeout(() => {
          executed = false;
          promiseRes(true);
        }, safeTimeoutConst * 1000);
      });

      await Promise.race([execute(), timeoutPromise]);

      if(!executed) {
        context.error('Execution timed out.');
        output = context.res.send('', 500, {});
      }
    } else {
        await execute();
    }
  } catch(e: any) {
    context.error(e.message.includes("Cannot resolve module") ? "Code file not found." : e.stack || e);
    output = context.res.send('', 500, {});
  } finally {
    console.log = stdlog;
    console.error = stderror;
    console.debug = stddebug;
    console.warn = stdwarn;
    console.info = stdinfo;
  }

  if(output === null || output === undefined) {
    context.error('Return statement missing. return context.res.empty() if no response is expected.');
    output = context.res.send('', 500, {});
  }

  output.body = output.body ?? '';
  output.statusCode = output.statusCode ?? 200;
  output.headers = output.headers ?? {};

  if (
    output.headers["content-type"] &&
    !output.headers["content-type"].startsWith("multipart/") &&
    !output.headers["content-type"].contains("charset=")
  ) {
    output.headers["content-type"] += "; charset=utf-8";
  }

  for (const header in output.headers) {
    if(header.toLowerCase().startsWith('x-open-runtimes-')) {
      continue;
    }
    
    ctx.response.headers.set(header.toLowerCase(), output.headers[header]);
  }

  if(customstd) {
    context.log('Unsupported log detected. Use context.log() or context.error() for logging.');
  }

  ctx.response.headers.set('x-open-runtimes-logs', encodeURIComponent(logs.join('\n')));
  ctx.response.headers.set('x-open-runtimes-errors', encodeURIComponent(errors.join('\n')));	

  ctx.response.status = output.statusCode;
  if(output.statusCode !== 204) {
    ctx.response.body = output.body;
  }
});

await app.listen({ port: 3000 });