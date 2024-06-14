import { Application } from "https://deno.land/x/oak@v10.6.0/mod.ts";
import { Logger } from "./logger.ts";

const USER_CODE_PATH = '/usr/local/server/src/function';

const app = new Application();

app.use(async (ctx: any) => {
  const logger = new Logger(ctx.request.headers.get('x-open-runtimes-logging'), ctx.request.headers.get('x-open-runtimes-log-id'));
  await logger.setup();

  try {
    await action(logger, ctx);
  } catch(e) {
    logger.write(e, Logger.TYPE_ERROR);

    ctx.response.headers.set('x-open-runtimes-log-id', logger.id);
    await logger.end();

    ctx.response.status = 500;
    ctx.response.body = '';
  }
});

const action = async (logger: Logger, ctx: any) => {
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

  if(Deno.env.get("OPEN_RUNTIMES_SECRET") && (ctx.request.headers.get("x-open-runtimes-secret") ?? '') !== Deno.env.get("OPEN_RUNTIMES_SECRET")) {
    ctx.response.status = 500;
    ctx.response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    return;
  }

  const contentType = ctx.request.headers.get('content-type') ?? 'text/plain';
  const bodyBinary: string = await ctx.request.body({ type: 'bytes' }).value;

  const headers: any = {};
  Array.from(ctx.request.headers.keys()).filter((header: any) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header: any) => {
    headers[header.toLowerCase()] = ctx.request.headers.get(header);
  });

  const enforcedHeaders = JSON.parse(Deno.env.get("OPEN_RUNTIMES_HEADERS") ? (Deno.env.get("OPEN_RUNTIMES_HEADERS") ?? '{}') : '{}');
  for(const header in enforcedHeaders) {
      headers[header.toLowerCase()] = `${enforcedHeaders[header]}`;
  }

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
      get body() {
        if(contentType.startsWith("application/json")) {
          return this.bodyJson;
        }

        const binaryTypes = ["application/", "audio/", "font/", "image/", "video/"];
        for(const type of binaryTypes) {
          if(contentType.startsWith(type)) {
            return this.bodyBinary;
          }
        }

        return this.bodyText;
      },
      get bodyRaw() {
        return this.bodyText;
      },
      get bodyText() {
        const decoder = new TextDecoder();
        return decoder.decode(this.bodyBinary);
      },
      get bodyJson() {
        return JSON.parse(this.bodyText);
      },
      get bodyBinary() {
        return bodyBinary;
      },
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
        return this.text(body, statusCode, headers);
      },
      text: function (body, statusCode = 200, headers = {}) {
        const encoder = new TextEncoder();
        return this.binary(encoder.encode(body), statusCode, headers);
      },
      binary: function(bytes, statusCode = 200, headers = {}) {
        return {
          body: bytes,
          statusCode: statusCode,
          headers: headers
        }
      },
      json: function (obj: any, statusCode = 200, headers: any = {}) {
        headers['content-type'] = 'application/json';
        return this.text(JSON.stringify(obj), statusCode, headers);
      },
      empty: function () {
        return this.text('', 204, {});
      },
      redirect: function (url: string, statusCode = 301, headers: any = {}) {
        headers['location'] = url;
        return this.text('', statusCode, headers);
      }
    },
    log: function (message: any) {
      logger.write(message, Logger.TYPE_LOG);
    },
    error: function (message: any) {
      logger.write(message, Logger.TYPE_ERROR);
    },
  };

  logger.overrideNativeLogs();

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
        output = context.res.text('', 500, {});
      }
    } else {
        await execute();
    }
  } catch(e: any) {
    context.error(e.message.includes("Cannot resolve module") ? "Code file not found." : e.stack || e);
    output = context.res.text('', 500, {});
  } finally {
    logger.revertNativeLogs();
  }

  if(output === null || output === undefined) {
    context.error('Return statement missing. return context.res.empty() if no response is expected.');
    output = context.res.text('', 500, {});
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

  const contentTypeValue =
    ctx.response.headers.get("content-type") ?? "text/plain";
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    ctx.response.headers.set(
      "content-type",
      contentTypeValue + "; charset=utf-8"
    );
  }

  ctx.response.headers.set('x-open-runtimes-log-id', logger.id);
  await logger.end();

  ctx.response.status = output.statusCode;
  if(output.statusCode !== 204) {
    ctx.response.body = output.body;
  }
};

await app.listen({ port: 3000 });