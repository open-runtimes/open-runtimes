const express = require("express");
const { Client } = require("pg");
const { spawn } = require("child_process");
const fs = require("fs");

const app = express();
const MANAGEMENT_PORT = 3000;

// Validate all required configuration
const requiredEnvVars = ['POSTGRES_USER', 'POSTGRES_PASSWORD', 'POSTGRES_DB'];
const missingVars = requiredEnvVars.filter(varName => !process.env[varName]);

if (missingVars.length > 0) {
  console.error("FATAL: Missing required environment variables:");
  missingVars.forEach(varName => console.error(`  - ${varName}`));
  console.error("\nAll database configuration must be provided at runtime.");
  console.error("Use Kubernetes Secret/ConfigMap or docker run -e");
  process.exit(1);
}

let postgresProcess = null;
let dbReady = false;
let pgClient = null;

function startPostgres() {
  console.log("Starting PostgreSQL server...");

  postgresProcess = spawn("docker-entrypoint.sh", ["postgres"], {
    env: {
      ...process.env,
    },
  });

  postgresProcess.stdout.on("data", (data) => {
    const message = data.toString();
    console.log(`[PostgreSQL] ${message}`);

    if (message.includes("database system is ready to accept connections")) {
      if (!dbReady) {
        dbReady = true;
        console.log("PostgreSQL is ready to accept connections");

        setTimeout(connectHealthClient, 2000);
      }
    }
  });

  postgresProcess.stderr.on("data", (data) => {
    const message = data.toString();
    console.error(`[PostgreSQL] ${message}`);

    if (message.includes("database system is ready to accept connections")) {
      if (!dbReady) {
        dbReady = true;
        console.log("PostgreSQL is ready to accept connections");

        setTimeout(connectHealthClient, 2000);
      }
    }
  });

  postgresProcess.on("close", (code) => {
    console.log(`PostgreSQL process exited with code ${code}`);
    dbReady = false;
  });
}

function connectHealthClient() {
  pgClient = new Client({
    host: "localhost",
    port: 5432,
    user: process.env.POSTGRES_USER,
    password: process.env.POSTGRES_PASSWORD,
    database: process.env.POSTGRES_DB,
  });

  pgClient.connect((err) => {
    if (err) {
      console.error("Failed to connect health check client:", err);
      pgClient = null;
      setTimeout(connectHealthClient, 5000);
    } else {
      console.log("Health check client connected");
    }
  });
}

app.get("/__opr/health", async (req, res) => {
  if (!dbReady) {
    return res.status(503).send("PostgreSQL is starting");
  }

  try {
    await pgClient.query("SELECT 1");
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

app.get("/__opr/status", (req, res) => {
  const status = {
    ready: dbReady,
    engine: "postgres",
    version: "16",
    uptime: process.uptime(),
    connections: {
      current: 0, // TODO: Query actual connection count
      max: parseInt(process.env.POSTGRES_MAX_CONNECTIONS || "100"),
    },
  };

  res.json(status);
});

process.on("SIGTERM", () => {
  console.log("Received SIGTERM, shutting down gracefully...");

  if (postgresProcess) {
    postgresProcess.kill("SIGTERM");
  }

  pgClient.end().then(() => {
    console.log("Shutdown complete");
    process.exit(0);
  });
});

process.on("SIGINT", () => {
  console.log("Received SIGINT, shutting down gracefully...");

  if (postgresProcess) {
    postgresProcess.kill("SIGINT");
  }

  pgClient.end().then(() => {
    console.log("Shutdown complete");
    process.exit(0);
  });
});

app.listen(MANAGEMENT_PORT, "0.0.0.0", () => {
  console.log(`OpenRuntimes management server listening on port ${MANAGEMENT_PORT}`);
  startPostgres();
});
