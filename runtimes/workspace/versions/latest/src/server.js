const micro = require("micro");
const { send } = require("micro");
const path = require("path");
const {
  Synapse,
  Terminal,
  Filesystem,
  System,
  Git,
  Code,
  Appwrite,
  Embeddings,
} = require("@appwrite.io/synapse");
const {
  HuggingFaceEmbeddingAdapter,
} = require("@appwrite.io/synapse/dist/adapters");
const { InputFile } = require("node-appwrite/file");

const WORK_DIR = process.env.WORK_DIR || "/tmp/workspace";

/**
 * @typedef {Object} Connection
 * @property {Terminal} terminal
 * @property {Filesystem} filesystem
 * @property {System} system
 * @property {Git} git
 * @property {Code} code
 * @property {Embeddings} embeddings
 * @property {Function[]} cleanupHandlers
 */

const synapse = new Synapse("localhost", process.env.PORT || 3000);
const huggingFaceAdapter = new HuggingFaceEmbeddingAdapter();

const appwrite = new Appwrite(synapse);

/** @type {Terminal} */
let globalTerminal;
/** @type {Filesystem} */
let globalFilesystem;
/** @type {System} */
let globalSystem;
/** @type {Git} */
let globalGit;
/** @type {Code} */
let globalCode;
/** @type {Embeddings} */
let globalEmbeddings;

/** @type {Map<string, Connection>} */
const connections = new Map(); // connectionId -> { terminal, filesystem, system, git, code }

/**
 * Get connection services for a given connection ID
 * @param {string} connectionId
 * @returns {Connection | null}
 */
function getConnection(connectionId) {
  return connections.get(connectionId) || null;
}

/**
 * Parse the URL and return the path and query parameters
 * @param {string} url
 * @returns {{ path: string, params: Record<string, string> }}
 */
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
  synapse: async (message, connectionId) => {
    if (!connectionId) {
      throw new Error("Method not allowed");
    }

    const connection = getConnection(connectionId);
    if (!connection) {
      throw new Error("Connection not found");
    }

    const {
      terminal: connectionTerminal,
      filesystem: connectionFilesystem,
      embeddings: connectionEmbeddings,
      git: connectionGit,
    } = connection;

    const { operation, params } = message;
    switch (operation) {
      case "updateWorkDir":
        if (!params.workDir) {
          throw new Error("Work directory not provided");
        }

        connectionTerminal.updateWorkDir(params.workDir);
        connectionFilesystem.updateWorkDir(params.workDir);
        connectionEmbeddings.updateWorkDir(params.workDir);
        connectionGit.updateWorkDir(params.workDir);

        return { success: true, data: "Work directory updated successfully" };
    }
  },

  terminal: async (message, connectionId) => {
    let terminal;
    if (connectionId) {
      const connection = getConnection(connectionId);
      terminal = connection?.terminal;
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
      case "executeCommand":
        const safeCwd = path.join(WORK_DIR, params.cwd);
        const result = await terminal.executeCommand({
          command: params.command,
          cwd: safeCwd,
          timeout: params.timeout,
        });
        return { success: true, data: result };
      default:
        throw new Error("Invalid terminal operation");
    }
    return null;
  },

  fs: async (message, connectionId) => {
    let filesystem;
    if (connectionId) {
      const connection = getConnection(connectionId);
      filesystem = connection?.filesystem;
    } else {
      filesystem = globalFilesystem;
    }
    if (!filesystem) throw new Error("Filesystem not initialized");
    const { operation, params } = message;
    let result;

    switch (operation) {
      case "createFile":
        result = await filesystem.createFile({
          filePath: params.filePath,
          content: params.content,
        });
        break;
      case "appendFile":
        result = await filesystem.appendFile({
          filePath: params.filePath,
          content: params.content,
        });
        break;
      case "getFile":
        result = await filesystem.getFile({
          filePath: params.filePath,
        });
        break;
      case "updateFile":
        result = await filesystem.updateFile({
          filePath: params.filePath,
          content: params.content,
        });
        break;
      case "updatefilePath":
        result = await filesystem.updatefilePath({
          oldPath: params.filePath,
          newPath: params.newPath,
        });
        break;
      case "listFilesInDir":
        result = await filesystem.listFilesInDir({
          dirPath: params.dirPath,
          withContent: params.withContent ?? false,
          recursive: params.recursive ?? false,
          additionalIgnorePatterns: params.additionalIgnorePatterns ?? [],
        });
        break;
      case "deleteFile":
        result = await filesystem.deleteFile({
          filePath: params.filePath,
        });
        break;
      case "createFolder":
        result = await filesystem.createFolder({
          dirPath: params.folderPath,
        });
        break;
      case "getFolder":
        result = await filesystem.getFolder({
          dirPath: params.folderPath,
        });
        break;
      case "updateFolderName":
        result = await filesystem.updateFolderName({
          dirPath: params.folderPath,
          name: params.name,
        });
        break;
      case "updatefolderPath":
        result = await filesystem.updatefolderPath({
          oldPath: params.folderPath,
          newPath: params.newPath,
        });
        break;
      case "deleteFolder":
        result = await filesystem.deleteFolder({
          dirPath: params.folderPath,
        });
        break;
      case "searchFiles":
        result = await filesystem.searchFiles({
          term: params.query,
        });
        break;
      default:
        throw new Error("Invalid operation");
    }
    return result;
  },

  system: async (message, connectionId) => {
    let system;
    if (connectionId) {
      const connection = getConnection(connectionId);
      system = connection?.system;
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
      const connection = getConnection(connectionId);
      git = connection?.git;
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
        result = await git.addRemote({
          name: params.name,
          url: params.url,
        });
        break;
      case "setUserName":
        result = await git.setUserName({
          name: params.name,
        });
        break;
      case "setUserEmail":
        result = await git.setUserEmail({
          email: params.email,
        });
        break;
      case "getCurrentBranch":
        result = await git.getCurrentBranch();
        break;
      case "status":
        result = await git.status();
        break;
      case "add":
        result = await git.add({
          files: params.files,
        });
        break;
      case "commit":
        result = await git.commit({
          message: params.message,
        });
        break;
      case "pull":
        result = await git.pull({
          branch: params.branch,
        });
        break;
      case "push":
        result = await git.push({
          branch: params.branch,
        });
        break;
      default:
        throw new Error("Invalid git operation");
    }
    return result;
  },

  code: async (message, connectionId) => {
    let code;
    if (connectionId) {
      const connection = getConnection(connectionId);
      code = connection?.code;
    } else {
      code = globalCode;
    }
    if (!code) throw new Error("Code not initialized");
    const { operation, params } = message;
    let result;

    switch (operation) {
      case "format":
        result = await code.format({
          code: params.code,
          options: params.options,
        });
        break;
      case "lint":
        result = await code.lint({
          code: params.code,
          options: params.options,
        });
        break;
      default:
        throw new Error("Invalid code operation");
    }
    return result;
  },

  appwrite: async (message) => {
    const { operation, service, params } = message;

    if (operation === "init") {
      if (!params.projectId || !params.jwt) {
        throw new Error("Endpoint, projectId, and jwt are required");
      }

      appwrite.init(params.endpoint, params.projectId, params.jwt);
      return {
        success: true,
        data: "Appwrite initialized successfully",
      };
    }

    if (service === "sites" && operation === "createDeployment") {
      await globalFilesystem.createGzipFile("code.tar.gz");
      const path = `${WORK_DIR}/code.tar.gz`;
      const file = InputFile.fromPath(path, "code.tar.gz");
      const result = await appwrite.call({
        service: "sites",
        method: "createDeployment",
        args: {
          siteId: params.siteId,
          code: file,
          activate: params.activate ?? false,
        },
      });
      return {
        success: true,
        data: result,
      };
    }

    const result = await appwrite.call({
      service,
      method: operation,
      args: params,
    });
    return {
      success: true,
      data: result,
    };
  },

  embeddings: async (message, connectionId) => {
    let embeddings;
    if (connectionId) {
      const connection = getConnection(connectionId);
      embeddings = connection?.embeddings;
    } else {
      embeddings = globalEmbeddings;
    }
    if (!embeddings) throw new Error("Embeddings not initialized");

    const urlParams = synapse.getParams(connectionId);
    if (!urlParams?.syncWorkDir || urlParams.syncWorkDir !== "true") {
      throw new Error("syncWorkDir must be enabled to use embeddings service");
    }

    const { operation, params } = message;
    let result;

    switch (operation) {
      case "getStats":
        result = {
          success: true,
          data: embeddings.getStats(),
        };
        break;
      case "findDocuments":
        result = await embeddings.findDocuments(params.query, params.limit);
        break;
      default:
        throw new Error("Invalid embeddings operation");
    }
    return result;
  },
};

synapse
  .connect("/")
  .then(() => {
    console.info("Synapse connected");

    // Initialize global service instances
    globalTerminal = new Terminal(synapse, {
      shell: "bash",
      workDir: WORK_DIR,
    });
    globalFilesystem = new Filesystem(synapse, WORK_DIR);
    globalSystem = new System(synapse);
    globalGit = new Git(synapse, WORK_DIR);
    globalCode = new Code(synapse);
    globalEmbeddings = new Embeddings(synapse, WORK_DIR, huggingFaceAdapter);

    synapse.onConnection((connectionId) => {
      console.info(`New Synapse connection: ${connectionId}`);
      console.info(
        `New Synapse connection: ${JSON.stringify(synapse.getConnection(connectionId))}`,
      );
      console.info(
        `All connections: ${JSON.stringify(synapse.getConnections())}`,
      );

      const urlParams = synapse.getParams(connectionId);
      let workDir = WORK_DIR;
      if (urlParams?.workDir && urlParams.workDir) {
        workDir = urlParams.workDir;
      }

      // Create new service instances for this connection
      const terminal = new Terminal(synapse, {
        shell: "bash",
        workDir,
      });
      const filesystem = new Filesystem(synapse, workDir);
      const system = new System(synapse);
      const git = new Git(synapse, workDir);
      const code = new Code(synapse);
      const embeddings = new Embeddings(synapse, workDir, huggingFaceAdapter);

      connections.set(connectionId, {
        terminal,
        filesystem,
        system,
        git,
        code,
        embeddings,
        cleanupHandlers: [],
      });

      // Terminal onData
      const terminalHandler = (success, data) => {
        if (synapse.isConnected(connectionId)) {
          synapse.send({
            connectionId,
            type: "terminal",
            payload: { success, data },
          });
        }
      };
      terminal.onData(terminalHandler);

      // Terminal onExit
      const terminalExitHandler = (success, exitCode, signal) => {
        if (synapse.isConnected(connectionId)) {
          synapse.send({
            connectionId,
            type: "terminalExit",
            payload: {
              success,
              exitCode,
              signal,
            },
          });
        }
      };
      terminal.onExit(terminalExitHandler);

      // Watch workdir if urlParams.syncWorkDir is true
      if (urlParams?.syncWorkDir && urlParams.syncWorkDir === "true") {
        const fsHandler = ({ path, event, content }) => {
          if (synapse.isConnected(connectionId)) {
            synapse.send({
              connectionId,
              type: "syncWorkDir",
              payload: {
                success: true,
                data: {
                  path,
                  event,
                  content,
                },
              },
            });
          }
        };
        filesystem.watchWorkDir(fsHandler);
        embeddings.startWatching();
      }

      // Store handlers for cleanup
      connections.get(connectionId).cleanupHandlers.push(() => {
        terminal.kill();
        filesystem.unwatchWorkDir();
        embeddings.dispose();
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
            synapse.send({
              connectionId,
              type,
              payload: {
                requestId: message.requestId,
                ...result,
              },
            });
          }
        } catch (error) {
          console.error(type, error);
          synapse.send({
            connectionId,
            type,
            payload: {
              requestId: message.requestId,
              success: false,
              error: error.message,
            },
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
    !globalCode ||
    !globalEmbeddings
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
    // update global services with new workdir
    globalTerminal.updateWorkDir(params.workDir);
    globalFilesystem.updateWorkDir(params.workDir);
    globalGit.updateWorkDir(params.workDir);
    globalEmbeddings.updateWorkDir(params.workDir);
  } else {
    globalTerminal.updateWorkDir(WORK_DIR);
    globalFilesystem.updateWorkDir(WORK_DIR);
    globalGit.updateWorkDir(WORK_DIR);
    globalEmbeddings.updateWorkDir(WORK_DIR);
  }

  if (method === "GET" && path === "/targz") {
    const tarGzResult = await globalFilesystem.createGzipFile();

    if (!tarGzResult.success) {
      return send(res, 500, {
        success: false,
        error: tarGzResult.error,
      });
    }

    res.setHeader("Content-Type", "application/gzip");
    res.setHeader(
      "Content-Disposition",
      'attachment; filename="download.tar.gz"',
    );
    res.setHeader("Content-Length", tarGzResult.data.buffer.length);

    res.statusCode = 200;
    res.end(tarGzResult.data.buffer);
    return;
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

      // special case for updateWorkDir
      if (type === "synapse" && operation === "updateWorkDir") {
        globalTerminal.updateWorkDir(params.workDir);
        globalFilesystem.updateWorkDir(params.workDir);
        globalGit.updateWorkDir(params.workDir);
        globalEmbeddings.updateWorkDir(params.workDir);

        return send(res, 200, {
          success: true,
          data: "Work directory updated successfully",
        });
      }

      const result = await router[type]({
        operation,
        params,
      });
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
  console.info(`New upgrade request from ${socket.remoteAddress}`);
  synapse.handleUpgrade(req, socket, head);
});
