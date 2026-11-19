const crypto = require("crypto");
const express = require("express");

// Answers the shared Serverless test actions (`x-action` header) the way an
// Express app would, so the Express helper runs the same suite as every runtime.

const app = express();

app.use(express.raw({ type: () => true, limit: "20mb" }));

const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const text = (res, body, status = 200, contentType = "text/plain; charset=utf-8") =>
  res.status(status).set("content-type", contentType).end(body);

function bodyOf(req) {
  return Buffer.isBuffer(req.body) ? req.body : Buffer.alloc(0);
}

function urlOf(req) {
  const scheme = req.headers["x-forwarded-proto"] ?? "http";
  const defaultPort = scheme === "https" ? "443" : "80";
  const [host, hostPort] = req.headers["host"].split(":");
  const port = +(hostPort ?? defaultPort);
  const [path, queryString = ""] = req.originalUrl.split("?");
  const query = {};
  for (const param of queryString.split("&")) {
    const [key, ...value] = param.split("=");
    if (key) {
      query[key] = value.join("=");
    }
  }

  return {
    url: `${scheme}://${host}${String(port) === defaultPort ? "" : `:${port}`}${path}${queryString === "" ? "" : `?${queryString}`}`,
    port,
    path,
    query,
    queryString,
    scheme,
    host,
  };
}

async function stream(res, chunks, status = 200, headers = {}) {
  res.status(status).set(headers);
  for await (const chunk of chunks) {
    res.write(chunk);
  }
  res.end();
}

const actions = {
  plaintextResponse: (req, res) => text(res, "Hello World 👋"),
  jsonResponse: (req, res) =>
    res.json({ json: true, message: "Developers are awesome." }),
  customCharsetResponse: (req, res) =>
    text(res, Buffer.from("ÅÆ", "latin1"), 200, "text/plain; charset=iso-8859-1"),
  uppercaseCharsetResponse: (req, res) => res.set("content-type", "TEXT/PLAIN").send("ÅÆ"),
  multipartResponse: (req, res) =>
    text(
      res,
      `--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--`,
      200,
      "multipart/form-data; boundary=12345",
    ),
  redirectResponse: (req, res) =>
    res.status(301).set("location", "https://github.com/").end(),
  emptyResponse: (req, res) => res.status(204).end(),
  headersResponse: (req, res) =>
    res
      .set({
        "first-header": "first-value",
        "second-header": req.headers["x-open-runtimes-custom-in-header"] ?? "missing",
        cookie: req.headers["cookie"] ?? "missing",
        "x-open-runtimes-custom-out-header": "third-value",
      })
      .type("text/plain")
      .send("OK"),
  statusResponse: (req, res) => text(res, "FAIL", 404),
  requestMethod: (req, res) => text(res, req.method),
  requestUrl: (req, res) => res.json(urlOf(req)),
  requestHeaders: (req, res) => res.json(req.headers),
  requestBodyText: (req, res) => text(res, bodyOf(req).toString("utf8")),
  requestBodyTextAuto: (req, res) => text(res, bodyOf(req).toString("utf8")),
  requestBodyJson: (req, res) => res.json(JSON.parse(bodyOf(req).toString("utf8"))),
  requestBodyJsonAuto: (req, res) => {
    const body = bodyOf(req).toString("utf8");
    res.json(body === "" ? {} : JSON.parse(body));
  },
  requestBodyBinary: (req, res) => res.type("application/octet-stream").send(bodyOf(req)),
  binaryResponse1: (req, res) => res.send(Buffer.from([0, 10, 255])),
  binaryResponse2: (req, res) => res.send(Buffer.from([0, 20, 255])),
  binaryResponse3: (req, res) => res.send(Buffer.from([0, 30, 255])),
  binaryResponse4: (req, res) => res.send(Buffer.from([0, 40, 255])),
  binaryResponse5: (req, res) => res.send(Buffer.from([0, 50, 255])),
  binaryResponseLarge: (req, res) =>
    res
      .set("x-method", req.method)
      .type("text/plain")
      .send(crypto.createHash("md5").update(bodyOf(req)).digest("hex")),
  streamResponse: (req, res) =>
    stream(
      res,
      (async function* () {
        console.log("Stream log.");
        yield "first\n";
        await wait(1000);
        yield "second\n";
        await wait(1000);
        yield "third\n";
      })(),
      201,
      { "x-stream": "yes" },
    ),
  streamException: (req, res) =>
    stream(
      res,
      (async function* () {
        yield "first\n";
        throw new Error("Stream failed");
      })(),
    ),
  streamTimeout: async (req, res) => {
    await wait(1000);
    await stream(
      res,
      (async function* () {
        yield "first\n";
        await wait(5000);
        yield "second\n";
      })(),
    );
  },
  streamEmpty: (req, res) => stream(res, []),
  streamSingle: (req, res) => stream(res, ["only\n"]),
  streamLong: (req, res) =>
    stream(
      res,
      (function* () {
        for (let i = 0; i < 10000; i++) {
          yield `chunk-${i}\n`;
        }
      })(),
    ),
  streamBinary: (req, res) =>
    stream(res, [
      Uint8Array.from({ length: 128 }, (_, i) => i),
      Uint8Array.from({ length: 128 }, (_, i) => i + 128),
    ]),
  streamJson: (req, res) =>
    stream(
      res,
      (async function* () {
        for (let n = 1; n <= 3; n++) {
          if (n > 1) {
            await wait(100);
          }
          yield JSON.stringify({ n }) + "\n";
        }
      })(),
      200,
      { "content-type": "application/x-ndjson" },
    ),
  streamErrorStart: (req, res) =>
    stream(
      res,
      (async function* () {
        throw new Error("Stream failed early");
      })(),
    ),
  envVars: (req, res) =>
    res.json({
      var: process.env.CUSTOM_ENV_VAR,
      emptyVar: process.env.NOT_DEFINED_VAR ?? null,
    }),
  logs: (req, res) => {
    console.log("Native log");
    console.log("Debug log");
    console.error("Error log");
    console.log("Log+With+Plus+Symbol");
    console.log(42);
    console.log(4.2);
    console.log(true);
    console.log({ objectKey: "objectValue" });
    console.log(["arrayValue"]);
    console.log(String(new Array(9000).fill("A")));
    console.error(String(new Array(9000).fill("B")));
    text(res, "");
  },
  library: async (req, res) => {
    const todo = await fetch(
      `https://dummyjson.com/todos/${bodyOf(req).toString("utf8")}`,
    ).then((r) => r.json());
    res.json({ todo });
  },
  timeout: async (req, res) => {
    console.log("Timeout start.");
    await wait(3000);
    console.log("Timeout end.");
    text(res, "Successful response.");
  },
  deprecatedMethods: (req, res) => text(res, bodyOf(req)),
  deprecatedMethodsUntypedBody: (req, res) => text(res, "50"),
  deprecatedMethodsBytesBody: (req, res) =>
    res
      .type("image/png")
      .send(
        Buffer.from(
          "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg==",
          "base64",
        ),
      ),
  enforcedHeaders: (req, res) =>
    res.json({
      "x-custom": req.headers["x-custom"],
      "x-custom-uppercase": req.headers["x-custom-uppercase"],
      "x-open-runtimes-custom": req.headers["x-open-runtimes-custom"],
    }),
  spreadOperatorLogs: (req, res) => {
    const engine = "open-runtimes";
    console.log("engine:", engine);
    console.error("engine:", engine);
    text(res, "OK");
  },
  setCookie: (req, res) =>
    res
      .set("set-cookie", ["cookie=value; path=/", "cookie2=value2; path=/"])
      .set("some-header", "some-value")
      .type("text/plain")
      .send("OK"),
  setCookie2: (req, res) => {
    res.append("set-cookie", "cookie=value; path=/");
    res.append("set-cookie", "cookie2=value2; path=/");
    res.set("some-header", "some-value").type("text/plain").send("OK");
  },
  // Responds from a timer, outside Express, so a response after the timeout
  // throws where nothing but the runtime can catch it.
  lateResponse: (req, res) => {
    setTimeout(() => text(res, "Late response."), 3000);
  },
  errorTest: () => {
    console.log("Before error...");
    throw new Error("Error!");
  },
};

// Express routing, beyond the shared actions
app.get("/users/:id", (req, res) => {
  res.json({ id: req.params.id, sort: req.query.sort });
});

app.use(async (req, res) => {
  const action = actions[req.headers["x-action"]];
  if (!action) {
    throw new Error("Unknown action");
  }

  await action(req, res);
});

// Errors go to the execution's error logs; the client gets an empty 500 (or the
// body parser's status, such as 413 for a body over the limit).
app.use((error, req, res, next) => {
  console.error(error);
  if (res.headersSent) {
    // Let the chunks already written reach the client, then abort.
    setImmediate(() => res.destroy());
    return;
  }

  res.status(error.status ?? 500).end();
});

app.listen(process.env.PORT, () => {
  console.log(`Express server started on http://0.0.0.0:${process.env.PORT}`);
});
