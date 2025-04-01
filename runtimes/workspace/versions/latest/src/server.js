const micro = require("micro");
const { send } = require("micro");
const {
  Synapse,
  Terminal,
  Filesystem,
  System,
} = require("@appwrite.io/synapse");

const workdir = "/usr/local/server/src/artifact";

const synapse = new Synapse();

synapse
  .connect("ws://localhost:3000/terminal")
  .then((synapse) => {
    console.log("Synapse connected");

    const terminal = new Terminal(synapse);
    const fs = new Filesystem(synapse);
    const system = new System(synapse);

    const router = {
      terminal: async (message) => {
        const { operation, params } = message;
        switch (operation) {
          case "updateSize":
            terminal.updateSize(params.cols, params.rows);
            break;
          case "createCommand":
            terminal.createCommand(params.command);
            break;
          default:
            throw new Error("Invalid terminal operation");
        }
        return null;
      },

      fs: async (message) => {
        const { operation, params } = message;
        let result;

        switch (operation) {
          case "createFile":
            result = await fs.createFile(
              workdir + "/" + params.filepath,
              params.content,
            );
            break;
          case "getFile":
            result = await fs.getFile(workdir + "/" + params.filepath);
            break;
          case "updateFile":
            result = await fs.updateFile(
              workdir + "/" + params.filepath,
              params.content,
            );
            break;
          case "updateFilePath":
            result = await fs.updateFilePath(
              workdir + "/" + params.filepath,
              workdir + "/" + params.newPath,
            );
            break;
          case "deleteFile":
            result = await fs.deleteFile(workdir + "/" + params.filepath);
            break;
          case "createFolder":
            result = await fs.createFolder(workdir + "/" + params.folderpath);
            break;
          case "getFolder":
            result = await fs.getFolder(workdir + "/" + params.folderpath);
            break;
          case "updateFolderName":
            result = await fs.updateFolderName(
              workdir + "/" + params.folderpath,
              params.name,
            );
            break;
          case "updateFolderPath":
            result = await fs.updateFolderPath(
              workdir + "/" + params.folderpath,
              workdir + "/" + params.newPath,
            );
            break;
          case "deleteFolder":
            result = await fs.deleteFolder(workdir + "/" + params.folderpath);
            break;
          default:
            throw new Error("Invalid operation");
        }
        return result;
      },

      system: async (message) => {
        const { operation } = message;
        let result;

        switch (operation) {
          case "getUsage":
            result = await system.getUsage();
            break;
          default:
            throw new Error("Invalid system operation");
        }
        return result;
      },
    };

    Object.keys(router).forEach((type) => {
      synapse.onMessageType(type, async (message) => {
        try {
          const result = await router[type](message);
          if (result !== null) {
            synapse.send(`${type}_response`, {
              requestId: message.requestId,
              ...result,
            });
          }
        } catch (error) {
          synapse.send(`${type}_response`, {
            requestId: message.requestId,
            success: false,
            error: error.message,
          });
        }
      });
    });

    synapse.onMessageType("terminal_input", (message) => {
      terminal.createCommand(message.data);
    });

    terminal.onData((data) => {
      synapse.send("terminal_output", { data });
    });

    synapse.onClose(() => {
      terminal.kill();
      console.log("Terminal connection closed");
    });
  })
  .catch((error) => {
    console.error("Failed to connect Synapse:", error);
  });

const server = micro(async (req, res) => {
  if (
    req.headers.upgrade &&
    req.headers.upgrade.toLowerCase() === "websocket"
  ) {
    // synapse.handleUpgrade(req, req.socket, Buffer.alloc(0));
    return;
  }
  return send(res, 404, "Not found");
});

const port = process.env.PORT || 3000;
server.listen(port, () => {
  console.log(`Terminal server running on port ${port}`);
});

server.on("connection", (socket) => {
  console.log(`New connection from ${socket.remoteAddress}`);
});
