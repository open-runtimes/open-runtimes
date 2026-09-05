// Shared by the Node function server and the bundled SSR adapters.
// Track sockets explicitly for Node versions predating closeIdleConnections().
module.exports = function installShutdown(server) {
  let stopping = false;
  const sockets = new Map();
  server.on("connection", (socket) => {
    sockets.set(socket, 0);
    socket.on("close", () => sockets.delete(socket));
  });
  server.prependListener("request", (req, res) => {
    const socket = req.socket;
    sockets.set(socket, (sockets.get(socket) || 0) + 1);
    res.once("finish", () => {
      const active = (sockets.get(socket) || 1) - 1;
      if (sockets.has(socket)) sockets.set(socket, active);
      if (stopping && active === 0) socket.end();
    });
  });
  function shutdown() {
    if (stopping) return;
    stopping = true;
    // The supervisor enforces the deadline even if user code blocks this loop.
    server.close(() => {
      process.stdout.write("", () => {
        process.stderr.write("", () => process.exit(0));
      });
    });
    for (const [socket, active] of sockets) {
      if (active === 0) socket.end();
    }
  }
  process.on("SIGTERM", shutdown);
  process.on("SIGINT", shutdown);
};
