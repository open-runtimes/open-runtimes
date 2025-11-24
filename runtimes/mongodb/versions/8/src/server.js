const express = require("express");
const { MongoClient } = require("mongodb");
const { spawn } = require("child_process");
const fs = require("fs");

const app = express();
const MANAGEMENT_PORT = 3000;

if (!process.env.MONGO_INITDB_ROOT_USERNAME) {
  console.warn("WARNING: MONGO_INITDB_ROOT_USERNAME not set, using default 'admin' (for testing only)");
  process.env.MONGO_INITDB_ROOT_USERNAME = 'admin';
}
if (!process.env.MONGO_INITDB_ROOT_PASSWORD) {
  console.warn("WARNING: MONGO_INITDB_ROOT_PASSWORD not set, using default 'admin' (for testing only)");
  process.env.MONGO_INITDB_ROOT_PASSWORD = 'admin';
}
if (!process.env.MONGO_INITDB_DATABASE) {
  console.warn("WARNING: MONGO_INITDB_DATABASE not set, using default 'admin' (for testing only)");
  process.env.MONGO_INITDB_DATABASE = 'admin';
}

let mongoProcess = null;
let dbReady = false;
let client = null;

function startMongoDB() {
  console.log("Starting MongoDB server...");

  const username = process.env.MONGO_INITDB_ROOT_USERNAME;
  const password = process.env.MONGO_INITDB_ROOT_PASSWORD;
  const database = process.env.MONGO_INITDB_DATABASE;

  const dbPath = "/data/db";
  const fs = require("fs");
  const needsInit = !fs.existsSync(`${dbPath}/mongod.lock`);

  if (needsInit) {
    console.log("MongoDB not initialized, will create root user on first start...");
  }

  const args = [
    "--bind_ip_all",
    "--auth",
  ];

  mongoProcess = spawn("mongod", args, {
    env: {
      ...process.env,
    },
  });

  let initAttempted = false;

  mongoProcess.stdout.on("data", (data) => {
    const message = data.toString();
    console.log(`[MongoDB] ${message}`);

    if (message.includes("Waiting for connections")) {
      if (!dbReady) {
        dbReady = true;
        console.log("MongoDB is ready to accept connections");

        if (needsInit && !initAttempted) {
          initAttempted = true;
          setTimeout(() => initializeRootUser(username, password, database), 2000);
        } else {
          setTimeout(connectHealthClient, 2000);
        }
      }
    }
  });

  mongoProcess.stderr.on("data", (data) => {
    const message = data.toString();
    console.error(`[MongoDB] ${message}`);
  });

  mongoProcess.on("close", (code) => {
    console.log(`MongoDB process exited with code ${code}`);
    dbReady = false;
  });
}

async function initializeRootUser(username, password, database) {
  try {
    console.log("Initializing root user...");

    const { MongoClient } = require("mongodb");
    const tempClient = new MongoClient("mongodb://localhost:27017/");
    await tempClient.connect();

    const adminDb = tempClient.db("admin");

    // Create root user
    await adminDb.command({
      createUser: username,
      pwd: password,
      roles: [{ role: "root", db: "admin" }]
    });

    console.log("Root user created successfully");

    if (database && database !== "admin") {
      await tempClient.db(database).createCollection("_init");
      console.log(`Database '${database}' created`);
    }

    await tempClient.close();

    setTimeout(connectHealthClient, 2000);
  } catch (err) {
    console.error("Failed to initialize root user:", err.message);
    if (err.codeName === "DuplicateKey" || err.code === 51003) {
      console.log("Root user already exists, continuing...");
      setTimeout(connectHealthClient, 2000);
    } else {
      setTimeout(() => initializeRootUser(username, password, database), 5000);
    }
  }
}

async function connectHealthClient() {
  try {
    const username = process.env.MONGO_INITDB_ROOT_USERNAME;
    const password = process.env.MONGO_INITDB_ROOT_PASSWORD;
    const authDb = process.env.MONGO_INITDB_DATABASE;

    const uri = `mongodb://${username}:${password}@localhost:27017/?authSource=${authDb}`;
    client = new MongoClient(uri);

    await client.connect();
    console.log("Health check client connected");

    await client.db("admin").command({ ping: 1 });
    console.log("MongoDB ping successful");
  } catch (err) {
    console.error("Failed to connect health check client:", err.message);
    client = null;
    setTimeout(connectHealthClient, 5000);
  }
}

app.get("/__opr/health", async (req, res) => {
  if (!dbReady) {
    return res.status(503).send("MongoDB is starting");
  }

  try {
    if (!client) {
      return res.status(503).send("Health check client not connected");
    }

    await client.db("admin").command({ ping: 1 });
    res.status(200).send("OK");
  } catch (err) {
    console.error("Health check failed:", err);
    res.status(503).send("Database unavailable");
  }
});

app.get("/__opr/timings", (req, res) => {
  try {
    const timings = fs.readFileSync("/mnt/telemetry/timings.txt", { encoding: "utf8" });
    res.setHeader("content-type", "text/plain; charset=utf-8");
    res.status(200).send(timings);
  } catch (err) {
    res.status(200).send("startup=0");
  }
});

app.get("/__opr/status", async (req, res) => {
  let currentConnections = 0;
  let availableConnections = 0;

  if (client && dbReady) {
    try {
      const serverStatus = await client.db("admin").command({ serverStatus: 1 });
      if (serverStatus && serverStatus.connections) {
        currentConnections = serverStatus.connections.current || 0;
        availableConnections = serverStatus.connections.available || 0;
      }
    } catch (err) {
      console.error("Failed to get connection count:", err);
    }
  }

  const status = {
    ready: dbReady,
    engine: "mongodb",
    version: "8",
    uptime: process.uptime(),
    connections: {
      current: currentConnections,
      available: availableConnections,
      max: currentConnections + availableConnections,
    },
  };

  res.json(status);
});

async function shutdown(signal) {
  console.log(`Received ${signal}, shutting down gracefully...`);

  if (client) {
    try {
      await client.close();
      console.log("Health check connection closed");
    } catch (err) {
      console.error("Error closing connection:", err);
    }
  }

  if (mongoProcess) {
    mongoProcess.kill("SIGTERM");
    setTimeout(() => {
      console.log("Shutdown complete");
      process.exit(0);
    }, 5000);
  } else {
    console.log("Shutdown complete");
    process.exit(0);
  }
}

process.on("SIGTERM", () => shutdown("SIGTERM"));
process.on("SIGINT", () => shutdown("SIGINT"));

// Start management server
app.listen(MANAGEMENT_PORT, "0.0.0.0", () => {
  console.log(`OpenRuntimes management server listening on port ${MANAGEMENT_PORT}`);

  // Start MongoDB
  startMongoDB();
});
