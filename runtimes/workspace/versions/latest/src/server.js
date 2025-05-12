const micro = require("micro");
const { send } = require("micro");
const {
  Synapse,
  Terminal,
  Filesystem,
  System,
  Git,
  Code,
} = require("@appwrite.io/synapse");

const workdir = "/tmp/workspace";

const synapse = new Synapse("localhost", 3000, workdir);

let globalTerminal, globalFilesystem, globalSystem, globalGit, globalCode; // initialize global services (for HTTP requests)
const connections = new Map(); // connectionId -> { terminal, filesystem, system, git, code }

function parseUrl(url) {
  const [path, query] = url.split("?");

  const params = {};
  if (query) {
    query.split("&").forEach((param) => {
      const [key, value] = param.split("=");
      params[key] = value;
    });
  }

  return {
    path,
    params,
  };
}

const router = {
  synapse: async (message) => {
    const { operation, params } = message;
    let result;

    switch (operation) {
      case "updateWorkDir":
        result = await synapse.updateWorkDir(params.workdir);
        break;
      default:
        throw new Error("Invalid synapse operation");
    }
    return result;
  },

  terminal: async (message, connectionId) => {
    let terminal;
    if (connectionId) {
      terminal = (connections.get(connectionId) || {}).terminal;
    } else {
      terminal = globalTerminal;
    }
    if (!terminal) throw new Error("Terminal not initialized");
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

  fs: async (message, connectionId) => {
    let filesystem;
    if (connectionId) {
      filesystem = (connections.get(connectionId) || {}).filesystem;
    } else {
      filesystem = globalFilesystem;
    }
    if (!filesystem) throw new Error("Filesystem not initialized");
    const { operation, params } = message;
    let result;

    switch (operation) {
      case "createFile":
        result = await filesystem.createFile(params.filepath, params.content);
        break;
      case "appendFile":
        result = await filesystem.appendFile(params.filepath, params.content);
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

  system: async (message, connectionId) => {
    let system;
    if (connectionId) {
      system = (connections.get(connectionId) || {}).system;
    } else {
      system = globalSystem;
    }
    if (!system) throw new Error("System not initialized");
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

  git: async (message, connectionId) => {
    let git;
    if (connectionId) {
      git = (connections.get(connectionId) || {}).git;
    } else {
      git = globalGit;
    }
    if (!git) throw new Error("Git not initialized");
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

  code: async (message, connectionId) => {
    let code;
    if (connectionId) {
      code = (connections.get(connectionId) || {}).code;
    } else {
      code = globalCode;
    }
    if (!code) throw new Error("Code not initialized");
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

synapse
  .connect("/")
  .then(() => {
    console.info("Synapse connected");

    // Initialize global service instances
    globalTerminal = new Terminal(synapse);
    globalFilesystem = new Filesystem(synapse);
    globalSystem = new System(synapse);
    globalGit = new Git(synapse);
    globalCode = new Code(synapse);

    synapse.onConnection((connectionId) => {
      console.info(`New Synapse connection: ${connectionId}`);
      console.info(
        `New Synapse connection: ${JSON.stringify(synapse.getConnection(connectionId))}`,
      );
      console.info(
        `All connections: ${JSON.stringify(synapse.getConnections())}`,
      );

      const urlParams = synapse.getParams(connectionId);
      if (urlParams?.workDir && urlParams.workDir !== synapse.workDir) {
        synapse.updateWorkDir(urlParams.workDir);
      }

      // Create new service instances for this connection
      const terminal = new Terminal(synapse);
      const filesystem = new Filesystem(synapse);
      const system = new System(synapse);
      const git = new Git(synapse);
      const code = new Code(synapse);

      connections.set(connectionId, {
        terminal,
        filesystem,
        system,
        git,
        code,
        cleanupHandlers: [],
      });

      // Terminal onData
      const terminalHandler = (success, data) => {
        if (synapse.isConnected(connectionId)) {
          synapse.send(connectionId, "terminal", { success, data });
        }
      };
      terminal.onData(terminalHandler);

      // Watch workdir
      const fsHandler = (success, data) => {
        if (synapse.isConnected(connectionId)) {
          synapse.send(connectionId, "syncWorkDir", { success, data });
        }
      };
      filesystem.watchWorkDir(fsHandler);

      // Store handlers for cleanup
      connections.get(connectionId).cleanupHandlers.push(() => {
        terminal.kill();
        filesystem.unwatchWorkDir();
      });
    });

    Object.keys(router).forEach((type) => {
      synapse.onMessageType(type, async (message, connectionId) => {
        if (!synapse.isConnected(connectionId)) {
          return;
        }
        try {
          const result = await router[type](message, connectionId);
          if (result !== null) {
            console.log(type, result);
            synapse.send(connectionId, type, {
              requestId: message.requestId,
              ...result,
            });
          }
        } catch (error) {
          console.error(type, error);
          synapse.send(connectionId, type, {
            requestId: message.requestId,
            success: false,
            error: error.message,
          });
        }
      });
    });

    synapse.onClose((connectionId, code, reason, wasClean) => {
      console.info(
        `Connection closed:\n  connectionId: ${connectionId}\n  code: ${code}\n  reason: ${reason || "N/A"}\n  wasClean: ${wasClean}`,
      );
      const conn = connections.get(connectionId);
      if (!conn) return;

      if (conn.terminal && conn.terminal.isTerminalAlive()) {
        conn.terminal.kill();
      }

      if (conn.cleanupHandlers) {
        conn.cleanupHandlers.forEach((fn) => fn());
      }

      connections.delete(connectionId);
    });
  })
  .catch((error) => {
    console.error("Failed to connect Synapse:", error);
  });

const server = micro(async (req, res) => {
  // Check if services are initialized
  if (
    !globalTerminal ||
    !globalFilesystem ||
    !globalSystem ||
    !globalGit ||
    !globalCode
  ) {
    return send(res, 503, {
      success: false,
      error: "Services not yet initialized. Please try again in a moment.",
    });
  }

  // Handle HTTP requests
  const { method, url } = req;
  const { path, params } = parseUrl(url);

  if (path === "/health") {
    return send(res, 200, { success: true, data: "OK" });
  }

  if (method === "GET" && path === "/") {
    return send(res, 200, {
      success: true,
      data: "Workspace runtime is running",
    });
  }

  if (params?.workDir) {
    synapse.updateWorkDir(params.workDir);
  }

  if (method === "POST" && path === "/") {
    try {
      const contentType = (req.headers["content-type"] || "").toLowerCase();
      if (!contentType.includes("application/json")) {
        return send(res, 400, {
          success: false,
          error: "Content-Type must be application/json",
        });
      }

      const rawBody = await micro.text(req);
      if (!rawBody || rawBody.trim() === "") {
        return send(res, 400, {
          success: false,
          error: "Request body is empty. Please provide a valid JSON payload.",
        });
      }

      let body;
      try {
        body = JSON.parse(rawBody);
      } catch (parseError) {
        return send(res, 400, {
          success: false,
          error: "Invalid JSON syntax. Please check your request body format.",
          details: parseError.message,
        });
      }

      const { type, operation, params } = body;

      if (!type || !operation) {
        return send(res, 400, {
          success: false,
          error: "Request must include 'type' and 'operation' fields",
        });
      }

      if (!router[type]) {
        return send(res, 400, {
          success: false,
          error: `Invalid type: ${type}`,
        });
      }

      const result = await router[type]({ operation, params });
      if (result && result.success === false) {
        return send(res, 400, { success: false, error: result.error });
      }
      return send(res, 200, { success: true, ...result });
    } catch (error) {
      return send(res, 400, { success: false, error: error.message });
    }
  }

  return send(res, 404, { success: false, error: "Not found" });
});

const port = process.env.PORT || 3000;
server.listen(port, () => {
  console.log(`Terminal server running on port ${port}`);
});

server.on("connection", (socket) => {
  console.info(`New connection from ${socket.remoteAddress}`);
});

server.on("upgrade", (req, socket, head) => {
  console.info(`New upgrade request from ${req.remoteAddress}`);
  synapse.handleUpgrade(req, socket, head);
});
