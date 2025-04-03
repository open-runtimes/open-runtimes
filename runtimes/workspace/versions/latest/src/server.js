const micro = require("micro");
const { send } = require("micro");
const {
  Synapse,
  Terminal,
  Filesystem,
  System,
  Git,
  CodeStyle,
} = require("@appwrite.io/synapse");

const workdir = "/tmp/workspace";

const synapse = new Synapse();

synapse
  .connect("/")
  .then((synapse) => {
    console.log("Synapse connected");
    console.log("Is synapse connected?", synapse.isConnected());

    const terminal = new Terminal(synapse);
    const fs = new Filesystem(synapse);
    const system = new System(synapse);
    const git = new Git(synapse);
    const codeStyle = new CodeStyle(synapse);

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

      git: async (message) => {
        const { operation, params } = message;
        let result;

        switch (operation) {
          case "init":
            result = await git.init();
            break;
          case "addRemote":
            result = await git.addRemote(params.name, params.url);
            break;
          case "getCurrentBranch":
            result = await git.getCurrentBranch();
            break;
          case "status":
            result = await git.status();
            break;
          case "add":
            result = await git.add(params.files);
            break;
          case "commit":
            result = await git.commit(params.message);
            break;
          case "pull":
            result = await git.pull();
            break;
          case "push":
            result = await git.push();
            break;
          default:
            throw new Error("Invalid git operation");
        }
        return result;
      },

      codeStyle: async (message) => {
        const { operation, params } = message;
        let result;

        switch (operation) {
          case "format":
            result = await codeStyle.format(params.code, params.options);
            break;
          case "lint":
            result = await codeStyle.lint(params.code, params.options);
            break;
          default:
            throw new Error("Invalid code style operation");
        }
        return result;
      },
    };

    Object.keys(router).forEach((type) => {
      synapse.onMessageType(type, async (message) => {
        console.log("Message received:", message);
        try {
          const result = await router[type](message);
          if (result !== null) {
            synapse.send(`${type}Response`, {
              requestId: message.requestId,
              ...result,
            });
          }
        } catch (error) {
          synapse.send(`${type}Response`, {
            requestId: message.requestId,
            success: false,
            error: error.message,
          });
        }
      });
    });

    terminal.onData((success, data) => {
      console.log("Sending terminal output:", data);
      synapse.send("terminalResponse", {
        success: success,
        data: data,
      });
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
  console.log("Request received:", req.headers);
  if (
    req.headers.upgrade &&
    req.headers.upgrade.toLowerCase() === "websocket"
  ) {
    synapse.handleUpgrade(req, req.socket, Buffer.alloc(0));
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
