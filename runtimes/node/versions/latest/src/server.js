const micro = require("micro");
const { buffer, send } = require("micro");
const Logger = require("./logger");

const USER_CODE_PATH = "/usr/local/server/src/function";

const server = micro(async (req, res) => {
  const logger = new Logger(
    req.headers[`x-open-runtimes-logging`],
    req.headers[`x-open-runtimes-log-id`]
  );

  try {
    await action(logger, req, res);
  } catch (e) {
    logger.write(e, Logger.TYPE_ERROR);

    res.setHeader("x-open-runtimes-log-id", logger.id);
    await logger.end();

    return send(res, 500, "");
  }
});

const action = async (logger, req, res) => {
  const timeout = req.headers[`x-open-runtimes-timeout`] ?? "";
  let safeTimeout = null;
  if (timeout) {
    if (isNaN(timeout) || timeout === 0) {
      return send(
        res,
        500,
        'Header "x-open-runtimes-timeout" must be an integer greater than 0.'
      );
    }

    safeTimeout = +timeout;
  }

  if (
    process.env["OPEN_RUNTIMES_SECRET"] &&
    req.headers[`x-open-runtimes-secret`] !==
      process.env["OPEN_RUNTIMES_SECRET"]
  ) {
    return send(
      res,
      500,
      'Unauthorized. Provide correct "x-open-runtimes-secret" header.'
    );
  }

  const contentType = (
    req.headers["content-type"] ?? "text/plain"
  ).toLowerCase();
  const bodyBinary = await buffer(req, { limit: "20mb" });

  const headers = {};
  Object.keys(req.headers)
    .filter((header) => !header.toLowerCase().startsWith("x-open-runtimes-"))
    .forEach((header) => {
      headers[header.toLowerCase()] = req.headers[header];
    });

  const enforcedHeaders = JSON.parse(
    process.env.OPEN_RUNTIMES_HEADERS ? process.env.OPEN_RUNTIMES_HEADERS : "{}"
  );
  for (const header in enforcedHeaders) {
    headers[header.toLowerCase()] = `${enforcedHeaders[header]}`;
  }

  const scheme = req.headers["x-forwarded-proto"] ?? "http";
  const defaultPort = scheme === "https" ? "443" : "80";
  const host = req.headers["host"].includes(":")
    ? req.headers["host"].split(":")[0]
    : req.headers["host"];
  const port = +(req.headers["host"].includes(":")
    ? req.headers["host"].split(":")[1]
    : defaultPort);
  const path = req.url.includes("?") ? req.url.split("?")[0] : req.url;
  const queryString = req.url.includes("?") ? req.url.split("?")[1] : "";
  const query = {};
  for (const param of queryString.split("&")) {
    let [key, ...valueArr] = param.split("=");
    const value = valueArr.join("=");

    if (key) {
      query[key] = value ?? "";
    }
  }

  const url = `${scheme}://${host}${port.toString() === defaultPort ? "" : `:${port}`}${path}${queryString === "" ? "" : `?${queryString}`}`;

  const context = {
    req: {
      get body() {
        if (contentType.startsWith("application/json")) {
          return this.bodyBinary && this.bodyBinary.length > 0
            ? this.bodyJson
            : {};
        }

        const binaryTypes = [
          "application/",
          "audio/",
          "font/",
          "image/",
          "video/",
        ];
        for (const type of binaryTypes) {
          if (contentType.startsWith(type)) {
            return this.bodyBinary;
          }
        }

        return this.bodyText;
      },
      get bodyRaw() {
        return this.bodyText;
      },
      get bodyText() {
        return this.bodyBinary.toString();
      },
      get bodyJson() {
        return JSON.parse(this.bodyText);
      },
      get bodyBinary() {
        return bodyBinary;
      },
      headers,
      method: req.method,
      host,
      scheme,
      query,
      queryString,
      port,
      url,
      path,
    },
    res: {
      send: function (body, statusCode = 200, headers = {}) {
        return this.text(`${body}`, statusCode, headers);
      },
      text: function (body, statusCode = 200, headers = {}) {
        return this.binary(Buffer.from(body, "utf8"), statusCode, headers);
      },
      binary: function (bytes, statusCode = 200, headers = {}) {
        return {
          body: bytes,
          statusCode: statusCode,
          headers: headers,
        };
      },
      json: function (obj, statusCode = 200, headers = {}) {
        headers["content-type"] = "application/json";
        return this.text(JSON.stringify(obj), statusCode, headers);
      },
      empty: function () {
        return this.text("", 204, {});
      },
      redirect: function (url, statusCode = 301, headers = {}) {
        headers["location"] = url;
        return this.text("", statusCode, headers);
      },
    },
    log: function (message) {
      logger.write(message, Logger.TYPE_LOG);
    },
    error: function (message) {
      logger.write(message, Logger.TYPE_ERROR);
    },
  };

  logger.overrideNativeLogs();

  let output = null;

  async function execute() {
    let userFunction;
    try {
      userFunction = require(
        USER_CODE_PATH + "/" + process.env.OPEN_RUNTIMES_ENTRYPOINT
      );
    } catch (err) {
      if (err.code === "ERR_REQUIRE_ESM") {
        userFunction = await import(
          USER_CODE_PATH + "/" + process.env.OPEN_RUNTIMES_ENTRYPOINT
        );
      } else {
        throw err;
      }
    }

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

    if (userFunction.default) {
      if (
        !(
          userFunction.default.constructor ||
          userFunction.default.call ||
          userFunction.default.apply
        )
      ) {
        throw new Error("User function is not valid.");
      }

      output = await userFunction.default(context);
    } else {
      output = await userFunction(context);
    }
  }

  try {
    if (safeTimeout !== null) {
      let executed = true;

      const timeoutPromise = new Promise((promiseRes) => {
        setTimeout(() => {
          executed = false;
          promiseRes(true);
        }, safeTimeout * 1000);
      });

      await Promise.race([execute(), timeoutPromise]);

      if (!executed) {
        context.error("Execution timed out.");
        output = context.res.text("", 500, {});
      }
    } else {
      await execute();
    }
  } catch (e) {
    if (e.code === "MODULE_NOT_FOUND") {
      context.error("Could not load code file.");
    }

    context.error(e.stack || e);
    output = context.res.text("", 500, {});
  } finally {
    logger.revertNativeLogs();
  }

  if (output === null || output === undefined) {
    context.error(
      "Return statement missing. return context.res.empty() if no response is expected."
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
    res.setHeader(header.toLowerCase(), output.headers[header]);
  }

  const contentTypeValue = (
    res.getHeader("content-type") ?? "text/plain"
  ).toLowerCase();
  if (
    !contentTypeValue.startsWith("multipart/") &&
    !contentTypeValue.includes("charset=")
  ) {
    res.setHeader("content-type", contentTypeValue + "; charset=utf-8");
  }

  res.setHeader("x-open-runtimes-log-id", logger.id);
  await logger.end();

  return send(res, output.statusCode, output.body);
};

server.listen(3000, undefined, undefined, () => {
    console.log("HTTP server successfully started!");
});
