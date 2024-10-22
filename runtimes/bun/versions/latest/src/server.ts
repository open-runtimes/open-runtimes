import { Logger } from "./logger";

const USER_CODE_PATH = "/usr/local/server/src/function";

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
    Bun.env["OPEN_RUNTIMES_SECRET"] &&
    (request.headers.get("x-open-runtimes-secret") ?? "") !==
      Bun.env["OPEN_RUNTIMES_SECRET"]
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

  const enforcedHeaders = JSON.parse(
    Bun.env["OPEN_RUNTIMES_HEADERS"] ? Bun.env["OPEN_RUNTIMES_HEADERS"] : "{}",
  );
  for (const header in enforcedHeaders) {
    headers[header.toLowerCase()] = `${enforcedHeaders[header]}`;
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

  async function execute() {
    const userFunction = (
      await import(USER_CODE_PATH + "/" + Bun.env["OPEN_RUNTIMES_ENTRYPOINT"])
    ).default;

    if (
      !(
        userFunction ||
        userFunction.constructor ||
        userFunction.call ||
        userFunction.apply
      )
    ) {
      throw new Error("User function is not valid.");
    }

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
    context.error(
      e.message.includes("Cannot resolve module")
        ? "Code file not found."
        : e.stack || e,
    );
    output = context.res.text("", 500, {});
  } finally {
    logger.revertNativeLogs();
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

  const responseHeaders: any = {};

  for (const header in output.headers) {
    if (header.toLowerCase().startsWith("x-open-runtimes-")) {
      continue;
    }

    responseHeaders[header.toLowerCase()] = output.headers[header];
  }

  const contentTypeValue = (
    responseHeaders["content-type"] ?? "text/plain"
  ).toLowerCase();
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    responseHeaders["content-type"] = contentTypeValue + "; charset=utf-8";
  }

  responseHeaders["x-open-runtimes-log-id"] = logger.id;
  await logger.end();

  return new Response(output.body, {
    status: output.statusCode,
    headers: responseHeaders,
  });
};

Bun.serve({
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
    );

    try {
      return await action(logger, request);
    } catch (e) {
      logger.write([e], Logger.TYPE_ERROR);

      const responseHeaders: any = {};
      responseHeaders["x-open-runtimes-log-id"] = logger.id;

      await logger.end();

      return new Response("", {
        status: 500,
        headers: responseHeaders,
      });
    }
  },
});

console.log("HTTP server successfully started!");
