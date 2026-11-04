import { config } from "./config";
import { Logger } from "./logger";

const USER_CODE_PATH = "/usr/local/server/src/function";

// With OPEN_RUNTIMES_LOGGING=inline, the body goes out in a `b` frame and the
// logs and errors follow in `l` and `e` frames, so edge splits them off the
// body. Each frame is its type, the payload's byte length in hex and a
// newline, then the payload.
const respond = async (
  logger: Logger,
  status: number,
  headers: Headers,
  body: any,
) => {
  if (!logger.inline) {
    return new Response(body, { status, headers });
  }

  // Framing gives the body a length of its own and needs a status that allows
  // a body, so the real ones travel in headers
  const bytes = new Uint8Array(await new Response(body).arrayBuffer());
  headers.delete("content-length");
  headers.set("x-open-runtimes-logs-inline", "enabled");
  headers.set("x-open-runtimes-content-length", `${bytes.length}`);
  if (status === 204 || status === 304) {
    headers.set("x-open-runtimes-status-code", `${status}`);
    status = 200;
  }

  const parts: Uint8Array[] = [];
  for (const [type, buffer] of [
    ["b", { chunks: [bytes], length: bytes.length }],
    ["l", logger.inlineLogs],
    ["e", logger.inlineErrors],
  ] as const) {
    if (buffer.length > 0) {
      parts.push(Buffer.from(`${type}${buffer.length.toString(16)}\n`));
      parts.push(...buffer.chunks);
    }
  }
  return new Response(Buffer.concat(parts), { status, headers });
};

const action = async (logger: Logger, request: any) => {
  const timeout = request.headers.get(`x-open-runtimes-timeout`) ?? "";
  let safeTimeout: number | null = null;
  if (timeout) {
    if (isNaN(+timeout) || timeout === "0") {
      return new Response(
        'Header "x-open-runtimes-timeout" must be an integer greater than 0.',
        {
          status: 500,
        },
      );
    }

    safeTimeout = +timeout;
  }

  if (
    config.secret &&
    (request.headers.get("x-open-runtimes-secret") ?? "") !== config.secret
  ) {
    return new Response(
      'Unauthorized. Provide correct "x-open-runtimes-secret" header.',
      {
        status: 500,
      },
    );
  }

  const contentType = (
    request.headers.get("content-type") ?? "text/plain"
  ).toLowerCase();
  const bodyBinary: string = await request.arrayBuffer();

  const headers: any = {};
  Array.from(request.headers.keys())
    .filter(
      (header: any) => !header.toLowerCase().startsWith("x-open-runtimes-"),
    )
    .forEach((header: any) => {
      headers[header.toLowerCase()] = request.headers.get(header);
    });

  for (const header in config.headers) {
    headers[header.toLowerCase()] = `${config.headers[header]}`;
  }

  const urlObject = new URL(request.url);

  const scheme = request.headers.get("x-forwarded-proto") ?? "http";
  const defaultPort = scheme === "https" ? "443" : "80";
  const hostHeader = request.headers.get("host") ?? "";
  const host = hostHeader.includes(":") ? hostHeader.split(":")[0] : hostHeader;
  const port = +(hostHeader.includes(":")
    ? hostHeader.split(":")[1]
    : defaultPort);
  const path = urlObject.pathname;
  const queryString = urlObject.href.includes("?")
    ? urlObject.href.split("?")[1]
    : "";
  const query: any = {};
  for (const param of queryString.split("&")) {
    let [key, ...valueArr] = param.split("=");
    const value = valueArr.join("=");

    if (key) {
      query[key] = value;
    }
  }

  const url = `${scheme}://${host}${port.toString() === defaultPort ? "" : `:${port}`}${path}${queryString === "" ? "" : `?${queryString}`}`;

  const context: any = {
    req: {
      get body() {
        if (contentType.startsWith("application/json")) {
          return this.bodyBinary && this.bodyBinary.byteLength > 0
            ? this.bodyJson
            : {};
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
      method: request.method,
      url,
      query,
      queryString,
      host,
      port,
      scheme,
      path,
    },
    res: {
      send: function (body: any, statusCode = 200, headers = {}) {
        return {
          body: body,
          statusCode: statusCode,
          headers: headers,
        };
      },
      text: function (body: string, statusCode = 200, headers = {}) {
        return this.binary(Buffer.from(body, "utf8"), statusCode, headers);
      },
      binary: function (bytes: any, statusCode = 200, headers = {}) {
        return {
          body: bytes,
          statusCode: statusCode,
          headers: headers,
        };
      },
      json: function (obj: any, statusCode = 200, headers: any = {}) {
        headers["content-type"] = "application/json";
        return this.text(JSON.stringify(obj), statusCode, headers);
      },
      empty: function () {
        return this.text("", 204, {});
      },
      redirect: function (url: string, statusCode = 301, headers: any = {}) {
        headers["location"] = url;
        return this.text("", statusCode, headers);
      },
    },
    log: function (...messages: any) {
      logger.write(messages, Logger.TYPE_LOG);
    },
    error: function (...messages: any) {
      logger.write(messages, Logger.TYPE_ERROR);
    },
  };

  logger.overrideNativeLogs();

  let output: any = null;
  let userFunction: any = null;

  const entrypoint = config.entrypoint;
  const entrypointFilePath = USER_CODE_PATH + "/" + entrypoint;
  const entrypointFile = Bun.file(entrypointFilePath);

  // Guard: Check file exists
  if (!(await entrypointFile.exists())) {
    context.error(
      `Failed to load entrypoint, file ${entrypoint} does not exist.`,
    );
    logger.revertNativeLogs();
    output = context.res.text("", 503, {});
  }

  // Guard: Try to load module
  if (output === null) {
    try {
      userFunction = (await import(entrypointFilePath)).default;

      if (typeof userFunction !== "function") {
        throw new TypeError(
          "Function signature invalid. Did you forget to export a 'main' function?",
        );
      }
    } catch (e: any) {
      const message = e instanceof Error ? e.message : String(e);
      if (e instanceof SyntaxError) {
        context.error(`Syntax error in ${entrypoint}: ${message}`);
      } else if (e instanceof TypeError) {
        context.error(message);
      } else {
        context.error(`Failed to load module: ${message}`);
      }
      logger.revertNativeLogs();
      output = context.res.text("", 503, {});
    }
  }

  // Execute user function
  if (output === null) {
    async function execute() {
      output = await userFunction(context);
    }

    try {
      if (safeTimeout !== null) {
        const safeTimeoutConst: number = safeTimeout;
        let executed = true;

        const timeoutPromise = new Promise((promiseRes) => {
          setTimeout(() => {
            executed = false;
            promiseRes(true);
          }, safeTimeoutConst * 1000);
        });

        await Promise.race([execute(), timeoutPromise]);

        if (!executed) {
          context.error("Execution timed out.");
          output = context.res.text("", 500, {});
        }
      } else {
        await execute();
      }
    } catch (e: any) {
      context.error(e.stack || e);
      output = context.res.text("", 500, {});
    } finally {
      logger.revertNativeLogs();
    }
  }

  if (output === null || output === undefined) {
    context.error(
      "Return statement missing. return context.res.empty() if no response is expected.",
    );
    output = context.res.text("", 500, {});
  }

  output.body = output.body ?? "";
  output.statusCode = output.statusCode ?? 200;
  output.headers = output.headers ?? {};

  const responseHeaders = new Headers();

  if (output.headers instanceof Headers) {
    for (const [key, value] of output.headers) {
      responseHeaders.append(key.toLowerCase(), value);
    }
  } else {
    for (const header in output.headers) {
      if (Array.isArray(output.headers[header])) {
        for (const value of output.headers[header]) {
          responseHeaders.append(header.toLowerCase(), value);
        }
      } else {
        responseHeaders.append(header.toLowerCase(), output.headers[header]);
      }
    }
  }

  for (const [key, _value] of responseHeaders) {
    if (key.toLowerCase().startsWith("x-open-runtimes-")) {
      responseHeaders.delete(key);
    }
  }

  const contentTypeValue = (
    responseHeaders.get("content-type") ?? "text/plain"
  ).toLowerCase();
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    responseHeaders.set("content-type", contentTypeValue + "; charset=utf-8");
  }

  responseHeaders.set("x-open-runtimes-log-id", logger.id);
  await logger.end();

  return respond(logger, output.statusCode, responseHeaders, output.body);
};

const server = Bun.serve({
  port: 3000,
  maxRequestBodySize: 20 * 1024 * 1024,
  idleTimeout: 0,
  async fetch(request) {
    const url = new URL(request.url);
    if (url.pathname === "/__opr/health") {
      return new Response("OK", {
        status: 200,
      });
    }
    if (url.pathname === "/__opr/timings") {
      return new Response(Bun.file("/mnt/telemetry/timings.txt"));
    }

    const logger = new Logger(
      request.headers.get("x-open-runtimes-logging"),
      request.headers.get("x-open-runtimes-log-id"),
      // A HEAD response has no body to carry them, so its logs are dropped
      config.inlineLogs && request.method !== "HEAD",
    );

    try {
      return await action(logger, request);
    } catch (e) {
      logger.write([e], Logger.TYPE_ERROR);

      const responseHeaders = new Headers();
      responseHeaders.set("x-open-runtimes-log-id", logger.id);

      await logger.end();

      return respond(logger, 500, responseHeaders, "");
    }
  },
});

console.log("HTTP server successfully started!");

let stopping = false;
async function shutdown() {
  if (stopping) return;
  stopping = true;
  await server.stop();
  // Bun 1.0/1.1 return void from stop(); newer versions return a Promise.
  // Older Bun can retain a stale pending count after rejecting a large upload.
  // Let the event loop exit naturally when only this poller remains.
  server.unref();
  const drained = setInterval(() => {
    if (server.pendingRequests === 0 && server.pendingWebSockets === 0) {
      process.exit(0);
    }
  }, 10);
  drained.unref();
}
process.on("SIGTERM", shutdown);
process.on("SIGINT", shutdown);
