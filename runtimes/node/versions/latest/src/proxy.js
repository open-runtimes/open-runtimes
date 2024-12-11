const httpProxy = require("http-proxy");
const { spawn } = require("child_process");

(async () => {
  const args = process.argv.slice(2);
  const child = spawn("sh", ["-c", args.join(" ")]);

  child.stdout.on("data", (data) => {
    console.log(data.toString());
  });

  child.stderr.on("data", (data) => {
    console.error(data.toString());
  });

  child.on("close", (code) => {
    process.exit(code);
  });

  // TODO: Only when port is open
  await new Promise((res) => setTimeout(res, 15000));

  const proxy = httpProxy.createProxyServer({
    target: {
      host: "127.0.0.1",
      port: 3001,
    },
  });

  proxy.on("proxyReq", function (proxyReq, req, res, options) {
    if (
      process.env["OPEN_RUNTIMES_SECRET"] &&
      req.headers[`x-open-runtimes-secret`] !==
        process.env["OPEN_RUNTIMES_SECRET"]
    ) {
      res.writeHead(500, { "Content-Type": "text/plain" });
      res.end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
      return;
    }
  });

  proxy.listen(3000);
})();
