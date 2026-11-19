// Express apps create their own server (app.listen), so no adapter installs the
// shared graceful shutdown for them like the bundled SSR servers do. Install it
// on the server bound to the runtime port once it listens: SIGTERM then lets
// active requests finish before the process exits. Other servers the app starts
// (a metrics port, for example) are left to the app.

import http from "node:http";
import installShutdown from "/usr/local/server/helpers/http-shutdown.cjs";

const port = Number(process.env.PORT);
const emit = http.Server.prototype.emit;

http.Server.prototype.emit = function (event, ...args) {
  if (event === "listening" && !this.__oprShutdownInstalled) {
    const address = this.address();
    if (
      address !== null &&
      typeof address === "object" &&
      address.port === port
    ) {
      this.__oprShutdownInstalled = true;
      installShutdown(this);
    }
  }

  return emit.call(this, event, ...args);
};
