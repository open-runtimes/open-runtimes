const express = require("express");
const mysql = require("mysql2/promise");
const { spawn } = require("child_process");
const fs = require("fs");

const app = express();
const MANAGEMENT_PORT = 3000;

// Validate all required configuration
const requiredEnvVars = ['MYSQL_ROOT_PASSWORD', 'MYSQL_DATABASE'];
const missingVars = requiredEnvVars.filter(varName => !process.env[varName]);

if (missingVars.length > 0) {
  console.error("FATAL: Missing required environment variables:");
  missingVars.forEach(varName => console.error(`  - ${varName}`));
  console.error("\nAll database configuration must be provided at runtime.");
  console.error("Use Kubernetes Secret/ConfigMap or docker run -e");
  process.exit(1);
}

let mysqlProcess = null;
let dbReady = false;
let connection = null;

// Start MySQL server
function startMySQL() {
  console.log("Starting MySQL server...");

  mysqlProcess = spawn("docker-entrypoint.sh", ["mysqld"], {
    env: {
      ...process.env,
    },
  });

  mysqlProcess.stdout.on("data", (data) => {
    const message = data.toString();
    console.log(`[MySQL] ${message}`);

    if (message.includes("ready for connections") || message.includes("socket: '/var/run/mysqld/mysqld.sock'")) {
      if (!dbReady) {
        dbReady = true;
        console.log("MySQL is ready to accept connections");

        setTimeout(connectHealthClient, 2000);
      }
    }
  });

  mysqlProcess.stderr.on("data", (data) => {
    const message = data.toString();
    console.error(`[MySQL] ${message}`);

    if (message.includes("ready for connections") || message.includes("port: 3306")) {
      if (!dbReady) {
        dbReady = true;
        console.log("MySQL is ready to accept connections");

        setTimeout(connectHealthClient, 2000);
      }
    }
  });

  mysqlProcess.on("close", (code) => {
    console.log(`MySQL process exited with code ${code}`);
    dbReady = false;
  });
}

async function connectHealthClient() {
  try {
    connection = await mysql.createConnection({
      host: "localhost",
      port: 3306,
      user: "root",
      password: process.env.MYSQL_ROOT_PASSWORD,
      database: process.env.MYSQL_DATABASE,
    });
    console.log("Health check client connected");
  } catch (err) {
    console.error("Failed to connect health check client:", err.message);
    // Retry after 5 seconds
    setTimeout(connectHealthClient, 5000);
  }
}

app.get("/__opr/health", async (req, res) => {
  if (!dbReady) {
    return res.status(503).send("MySQL is starting");
  }

  try {
    if (!connection) {
      return res.status(503).send("Health check client not connected");
    }

    await connection.query("SELECT 1");
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

  if (connection && dbReady) {
    try {
      const [rows] = await connection.query("SHOW STATUS LIKE 'Threads_connected'");
      if (rows && rows.length > 0) {
        currentConnections = parseInt(rows[0].Value);
      }
    } catch (err) {
      console.error("Failed to get connection count:", err);
    }
  }

  const status = {
    ready: dbReady,
    engine: "mysql",
    version: "8.4",
    uptime: process.uptime(),
    connections: {
      current: currentConnections,
      max: parseInt(process.env.MYSQL_MAX_CONNECTIONS || "151"),
    },
  };

  res.json(status);
});

async function shutdown(signal) {
  console.log(`Received ${signal}, shutting down gracefully...`);

  if (connection) {
    try {
      await connection.end();
      console.log("Health check connection closed");
    } catch (err) {
      console.error("Error closing connection:", err);
    }
  }

  if (mysqlProcess) {
    mysqlProcess.kill("SIGTERM");
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

app.listen(MANAGEMENT_PORT, "0.0.0.0", () => {
  console.log(`OpenRuntimes management server listening on port ${MANAGEMENT_PORT}`);
  startMySQL();
});
