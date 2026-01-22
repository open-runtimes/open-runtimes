import { Logger } from "./logger.ts";
import { getContextBody } from "./getContextBody.ts";

const USER_CODE_PATH = "/usr/local/server/src/function";

export const action = async (logger: Logger, ctx: any) => {
  const timeout = ctx.request.headers.get(`x-open-runtimes-timeout`) ?? "";
  let safeTimeout: number | null = null;
  if (timeout) {
    if (isNaN(+timeout) || timeout === "0") {
      ctx.response.status = 500;
      ctx.response.body =
        'Header "x-open-runtimes-timeout" must be an integer greater than 0.';
      return;
    }

    safeTimeout = +timeout;
  }

  if (
    Deno.env.get("OPEN_RUNTIMES_SECRET") &&
    (ctx.request.headers.get("x-open-runtimes-secret") ?? "") !==
      Deno.env.get("OPEN_RUNTIMES_SECRET")
  ) {
    ctx.response.status = 500;
    ctx.response.body =
      'Unauthorized. Provide correct "x-open-runtimes-secret" header.';
    return;
  }

  const maxSize = 20 * 1024 * 1024;

  const contentLength = ctx.request.headers.get("content-length") ?? "0";
  if (+contentLength > maxSize) {
    throw new Error("Request body size exceeds the size limit.");
  }

  const contentType = (ctx.request.headers.get("content-type") ?? "text/plain")
    .toLowerCase();

  const bodyBinary: any = await getContextBody(ctx);

  if (bodyBinary.byteLength > maxSize) {
    throw new Error("Request body size exceeds the size limit.");
  }

  const headers: any = {};
  Array.from(ctx.request.headers.keys()).filter((header: any) =>
    !header.toLowerCase().startsWith("x-open-runtimes-")
  ).forEach((header: any) => {
    headers[header.toLowerCase()] = ctx.request.headers.get(header);
  });

  const enforcedHeaders = JSON.parse(
    Deno.env.get("OPEN_RUNTIMES_HEADERS")
      ? (Deno.env.get("OPEN_RUNTIMES_HEADERS") ?? "{}")
      : "{}",
  );
  for (const header in enforcedHeaders) {
    headers[header.toLowerCase()] = `${enforcedHeaders[header]}`;
  }

  const scheme = ctx.request.headers.get("x-forwarded-proto") ?? "http";
  const defaultPort = scheme === "https" ? "443" : "80";
  const hostHeader = ctx.request.headers.get("host") ?? "";
  const host = hostHeader.includes(":") ? hostHeader.split(":")[0] : hostHeader;
  const port =
    +(hostHeader.includes(":") ? hostHeader.split(":")[1] : defaultPort);
  const path = ctx.request.url.pathname;
  const queryString = ctx.request.url.href.includes("?")
    ? ctx.request.url.href.split("?")[1]
    : "";
  const query: any = {};
  for (const param of queryString.split("&")) {
    let [key, ...valueArr] = param.split("=");
    const value = valueArr.join("=");

    if (key) {
      query[key] = value;
    }
  }

  const url = `${scheme}://${host}${
    port.toString() === defaultPort ? "" : `:${port}`
  }${path}${queryString === "" ? "" : `?${queryString}`}`;

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
      method: ctx.request.method,
      url,
      query,
      queryString,
      host,
      port,
      scheme,
      path,
    },
    res: {
      send: function (body: any, statusCode = 200, headers: any = {}) {
        return {
          body: body,
          statusCode: statusCode,
          headers: headers,
        };
      },
      text: function (body: string, statusCode = 200, headers = {}) {
        const encoder = new TextEncoder();
        return this.binary(encoder.encode(body), statusCode, headers);
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

  const entrypoint = Deno.env.get("OPEN_RUNTIMES_ENTRYPOINT") ?? "";
  const entrypointFilePath = USER_CODE_PATH + "/" + entrypoint;

  // Guard: Check file exists
  let fileExists = false;
  try {
    const stat = await Deno.stat(entrypointFilePath);
    fileExists = stat.isFile;
  } catch {
    fileExists = false;
  }

  if (!fileExists) {
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

  for (const header in output.headers) {
    if (header.toLowerCase().startsWith("x-open-runtimes-")) {
      continue;
    }

    ctx.response.headers.set(header.toLowerCase(), output.headers[header]);
  }

  const contentTypeValue =
    (ctx.response.headers.get("content-type") ?? "text/plain").toLowerCase();
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    ctx.response.headers.set(
      "content-type",
      contentTypeValue + "; charset=utf-8",
    );
  }

  ctx.response.headers.set("x-open-runtimes-log-id", logger.id);
  await logger.end();

  ctx.response.status = output.statusCode;
  if (output.statusCode !== 204) {
    ctx.response.body = output.body;
  }
};
