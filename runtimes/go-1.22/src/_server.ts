const USER_CODE_PATH = '/usr/local/server/src/function';

const action = async (request) => {
  const timeout = request.headers.get(`x-open-runtimes-timeout`) ?? '';
  let safeTimeout: number | null = null;
  if(timeout) {
      if(isNaN(+timeout) || timeout === '0') {
          return new Response('Header "x-open-runtimes-timeout" must be an integer greater than 0.', {
            status: 500
          });
      }

      safeTimeout = +timeout;
  }

  if ((request.headers.get("x-open-runtimes-secret") ?? '')  === '' || (request.headers.get("x-open-runtimes-secret") ?? '') !== (Bun.env["OPEN_RUNTIMES_SECRET"] ?? '')) {
    return new Response('Unauthorized. Provide correct "x-open-runtimes-secret" header.', {
      status: 500
    });
  }
  const logs: string[] = [];
  const errors: string[] = [];

  const contentType = request.headers.get('content-type') ?? 'text/plain';
  const bodyRaw: string = await request.text();
  let body: any = bodyRaw;

  if (contentType.includes('application/json')) {
    if(bodyRaw) {
      body = JSON.parse(body);
    } else {
      body = {};
    }
  }

  const headers: any = {};
  Array.from(request.headers.keys()).filter((header: any) => !header.toLowerCase().startsWith('x-open-runtimes-')).forEach((header: any) => {
    headers[header.toLowerCase()] = request.headers.get(header);
  });

  const urlObject = new URL(request.url);

  const scheme = request.headers.get('x-forwarded-proto') ?? 'http';
  const defaultPort = scheme === 'https' ? '443' : '80';
  const hostHeader = request.headers.get('host') ?? '';
  const host = hostHeader.includes(':') ? hostHeader.split(':')[0] : hostHeader;
  const port = +(hostHeader.includes(':') ? hostHeader.split(':')[1] : defaultPort);
  const path = urlObject.pathname;
  const queryString = urlObject.href.includes('?') ? urlObject.href.split('?')[1] : '';
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
      method: request.method,
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
        return {
          body: body,
          statusCode: statusCode,
          headers: headers
        }
      },
      json: function (obj: any, statusCode = 200, headers: any = {}) {
        headers['content-type'] = 'application/json';
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
  console.log = console.info = console.debug = console.warn = console.error = function(...args: any[]) {
    const formattedArgs = args.map(arg => typeof arg === 'object' ? JSON.stringify(arg) : arg);
    customstd += formattedArgs.join(' ') + '\n';
  }

  let output: any = null;

  async function execute() {
    const userFunction = (await import(USER_CODE_PATH + '/' + Bun.env["OPEN_RUNTIMES_ENTRYPOINT"])).default;

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

  const responseHeaders: any = {};

  for (const header in output.headers) {
    if(header.toLowerCase().startsWith('x-open-runtimes-')) {
      continue;
    }
    
    responseHeaders[header.toLowerCase()] = output.headers[header];
  }

  const contentTypeValue = responseHeaders["content-type"] ?? "text/plain";
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    responseHeaders["content-type"] = contentTypeValue + "; charset=utf-8";
  }

  if(customstd) {
    context.log('');
    context.log('----------------------------------------------------------------------------');
    context.log('Unsupported logs detected. Use context.log() or context.error() for logging.');
    context.log('----------------------------------------------------------------------------');
    context.log(customstd);
    context.log('----------------------------------------------------------------------------');
  }

  responseHeaders['x-open-runtimes-logs'] = encodeURIComponent(logs.join('\n'));
  responseHeaders['x-open-runtimes-errors'] = encodeURIComponent(errors.join('\n'));

  return new Response(output.body, {
    status: output.statusCode,
    headers: responseHeaders
  });
};
