/**
 * Proxy server used for port-forwarding to SSR server
 * Used when SSR builds to standalone process, to preserve logs and auth check properly
 **/

import { createProxyServer } from "http-proxy";
import { spawn } from "child_process";
import { Socket } from "net";

(async () => {
  const command = process.argv.slice(2).join(" ");
  startServerProcess(command);

  await waitForPort(3001);

  startProxy(3000, 3001);
})();

function startProxy(portProxy, portTarget) {
  const proxy = createProxyServer({
    target: {
      host: "127.0.0.1",
      port: portTarget,
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

  proxy.listen(portProxy);
}

function startServerProcess(command) {
  const child = spawn("sh", ["-c", command]);

  child.stdout.on("data", (data) => {
    console.log(data.toString());
  });

  child.stderr.on("data", (data) => {
    console.error(data.toString());
  });

  child.on("close", (code) => {
    process.exit(code);
  });
}

function waitForPort(port, interval = 100, timeout = 900000) {
  return new Promise((resolve, reject) => {
    const checkPort = () => {
      const socket = new Socket();
      socket.setTimeout(100);

      socket.on("connect", () => {
        socket.destroy();
        resolve();
      });

      socket.on("timeout", () => {
        socket.destroy();
      });

      socket.on("error", (err) => {
        socket.destroy();
        if (err.code !== "ECONNREFUSED") {
          reject(err);
        }
      });

      socket.connect(port, "127.0.0.1");
    };

    const intervalId = setInterval(() => {
      checkPort();
    }, interval);

    setTimeout(() => {
      clearInterval(intervalId);
      reject(new Error(`Port ${port} did not open in time.`));
    }, timeout);
  });
}
