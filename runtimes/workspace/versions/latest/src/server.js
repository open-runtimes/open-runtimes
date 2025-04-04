const micro = require("micro");
const { send } = require("micro");
const fs = require("fs");
const {
  Synapse,
  Terminal,
  Filesystem,
  System,
  Git,
  Code,
} = require("@appwrite.io/synapse");

const workdir = "/tmp/workspace";

if (!fs.existsSync(workdir)) {
  fs.mkdirSync(workdir, { recursive: true });
}

// Initialize services
let terminal, filesystem, system, git, code;
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
        result = await filesystem.createFile(params.filepath, params.content);
        break;
      case "getFile":
        result = await filesystem.getFile(params.filepath);
        break;
      case "updateFile":
        result = await filesystem.updateFile(params.filepath, params.content);
        break;
      case "updateFilePath":
        result = await filesystem.updateFilePath(
          params.filepath,
          params.newPath,
        );
        break;
      case "deleteFile":
        result = await filesystem.deleteFile(params.filepath);
        break;
      case "createFolder":
        result = await filesystem.createFolder(params.folderpath);
        break;
      case "getFolder":
        result = await filesystem.getFolder(params.folderpath);
        break;
      case "updateFolderName":
        result = await filesystem.updateFolderName(
          params.folderpath,
          params.name,
        );
        break;
      case "updateFolderPath":
        result = await filesystem.updateFolderPath(
          params.folderpath,
          params.newPath,
        );
        break;
      case "deleteFolder":
        result = await filesystem.deleteFolder(params.folderpath);
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
      case "setUserName":
        result = await git.setUserName(params.name);
        break;
      case "setUserEmail":
        result = await git.setUserEmail(params.email);
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

  code: async (message) => {
    const { operation, params } = message;
    let result;

    switch (operation) {
      case "format":
        result = await code.format(params.code, params.options);
        break;
      case "lint":
        result = await code.lint(params.code, params.options);
        break;
      default:
        throw new Error("Invalid code operation");
    }
    return result;
  },
};

const synapse = new Synapse("localhost", 3000, workdir);

synapse
  .connect("/")
  .then((synapse) => {
    console.log("Synapse connected");
    console.log("Is synapse connected?", synapse.isConnected());

    // Initialize service instances
    terminal = new Terminal(synapse);
    filesystem = new Filesystem(synapse);
    system = new System(synapse);
    git = new Git(synapse);
    code = new Code(synapse);

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
      if (synapse.isConnected()) {
        console.log("Sending terminal output:", data);
        synapse.send("terminalResponse", {
          success: success,
          data: data,
        });
      }
    });

    synapse.onClose(() => {
      console.log("Terminal connection closed");
    });
  })
  .catch((error) => {
    console.error("Failed to connect Synapse:", error);
  });

const server = micro(async (req, res) => {
  console.log("Request received:", req.headers);

  // Handle WebSocket upgrade requests
  if (
    req.headers.upgrade &&
    req.headers.upgrade.toLowerCase() === "websocket"
  ) {
    synapse.handleUpgrade(req, req.socket, Buffer.alloc(0));
    return;
  }

  // Check if services are initialized
  if (!terminal || !filesystem || !system || !git || !code) {
    return send(res, 503, {
      success: false,
      error: "Services not yet initialized. Please try again in a moment.",
    });
  }

  // Basic routing for HTTP requests
  try {
    const { method, url } = req;

    // Parse request body for POST requests
    let body = {};
    if (method === "POST") {
      const contentType = (req.headers["content-type"] || "").toLowerCase();

      if (contentType.includes("application/json")) {
        try {
          const rawBody = await micro.text(req);
          if (!rawBody || rawBody.trim() === "") {
            return send(res, 400, {
              success: false,
              error:
                "Request body is empty. Please provide a valid JSON payload.",
            });
          }

          try {
            body = JSON.parse(rawBody);
          } catch (parseError) {
            return send(res, 400, {
              success: false,
              error:
                "Invalid JSON syntax. Please check your request body format.",
              details: parseError.message,
            });
          }
        } catch (error) {
          return send(res, 500, {
            success: false,
            error: "Error reading request body",
            details: error.message,
          });
        }
      } else {
        try {
          // For non-JSON content types, get raw body
          body = await micro.text(req);
        } catch (error) {
          return send(res, 400, {
            success: false,
            error: "Error reading request body",
            details: error.message,
          });
        }
      }
    }

    // Health endpoint
    if (url === "/health") {
      return send(res, 200, { success: true, data: "OK" });
    }

    // Terminal endpoints
    if (url.startsWith("/terminal/")) {
      const operation = url.split("/")[2];
      if (method === "POST") {
        try {
          await router.terminal({ operation, params: body });
          return send(res, 200, { success: true });
        } catch (error) {
          return send(res, 400, { success: false, error: error.message });
        }
      }
    }

    // Filesystem endpoints
    if (url.startsWith("/fs/")) {
      const operation = url.split("/")[2];
      if (method === "POST") {
        try {
          const result = await router.fs({ operation, params: body });
          return send(res, 200, { success: true, ...result });
        } catch (error) {
          return send(res, 400, { success: false, error: error.message });
        }
      }
    }

    // System endpoints
    if (url.startsWith("/system/")) {
      const operation = url.split("/")[2];
      if (method === "GET" && operation === "getUsage") {
        try {
          const result = await router.system({ operation });
          return send(res, 200, { success: true, ...result });
        } catch (error) {
          return send(res, 400, { success: false, error: error.message });
        }
      }
    }

    // Git endpoints
    if (url.startsWith("/git/")) {
      const operation = url.split("/")[2];
      if (method === "POST") {
        try {
          const result = await router.git({ operation, params: body });
          return send(res, 200, { success: true, ...result });
        } catch (error) {
          return send(res, 400, { success: false, error: error.message });
        }
      }
    }

    // Code endpoints
    if (url.startsWith("/code/")) {
      const operation = url.split("/")[2];
      if (method === "POST") {
        try {
          const result = await router.code({ operation, params: body });
          return send(res, 200, { success: true, ...result });
        } catch (error) {
          return send(res, 400, { success: false, error: error.message });
        }
      }
    }

    return send(res, 404, { success: false, error: "Not found" });
  } catch (error) {
    console.error("Error handling request:", error);
    return send(res, 500, { success: false, error: "Internal server error" });
  }
});

const port = process.env.PORT || 3000;
server.listen(port, () => {
  console.log(`Terminal server running on port ${port}`);
});

server.on("connection", (socket) => {
  console.log(`New connection from ${socket.remoteAddress}`);
});
