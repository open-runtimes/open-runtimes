const express = require("express");
const { Client } = require("pg");
const { spawn } = require("child_process");
const fs = require("fs");

const app = express();
const MANAGEMENT_PORT = 3000;

if (!process.env.POSTGRES_USER) {
  console.warn("WARNING: POSTGRES_USER not set, using default 'postgres' (for testing only)");
  process.env.POSTGRES_USER = 'postgres';
}
if (!process.env.POSTGRES_PASSWORD) {
  console.warn("WARNING: POSTGRES_PASSWORD not set, using default 'postgres' (for testing only)");
  process.env.POSTGRES_PASSWORD = 'postgres';
}
if (!process.env.POSTGRES_DB) {
  console.warn("WARNING: POSTGRES_DB not set, using default 'postgres' (for testing only)");
  process.env.POSTGRES_DB = 'postgres';
}

let postgresProcess = null;
let dbReady = false;
let pgClient = null;
let startupTimingWritten = false;

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

        // Write startup timing if provided
        if (process.env.STARTUP_TIME && !startupTimingWritten) {
          const start = parseFloat(process.env.STARTUP_TIME);
          const end = parseFloat(fs.readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
          const elapsed = (end - start).toFixed(3);
          fs.appendFileSync('/mnt/telemetry/timings.txt', `startup=${elapsed}\n`);
          startupTimingWritten = true;
          console.log(`Recorded startup timing: ${elapsed}s`);
        }

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

        // Write startup timing if provided
        if (process.env.STARTUP_TIME && !startupTimingWritten) {
          const start = parseFloat(process.env.STARTUP_TIME);
          const end = parseFloat(fs.readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
          const elapsed = (end - start).toFixed(3);
          fs.appendFileSync('/mnt/telemetry/timings.txt', `startup=${elapsed}\n`);
          startupTimingWritten = true;
          console.log(`Recorded startup timing: ${elapsed}s`);
        }

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
  let volumeInfo = {};

  // Check volume status
  try {
    const { execSync } = require("child_process");

    // Check data directory
    const pgDataPath = process.env.PGDATA || "/var/lib/postgresql/data";
    if (fs.existsSync(pgDataPath)) {
      const dfOutput = execSync(`df -h "${pgDataPath}" 2>/dev/null || echo ""`).toString();
      const lines = dfOutput.trim().split('\n');
      if (lines.length > 1) {
        const parts = lines[1].split(/\s+/);
        if (parts.length >= 5) {
          volumeInfo.data = {
            path: pgDataPath,
            size: parts[1],
            used: parts[2],
            available: parts[3],
            usePercent: parts[4],
            mounted: true
          };
        }
      }
    } else {
      volumeInfo.data = { path: pgDataPath, mounted: false };
    }

    // Check backup directory
    if (fs.existsSync("/mnt/backups")) {
      const dfOutput = execSync("df -h /mnt/backups 2>/dev/null || echo ''").toString();
      const lines = dfOutput.trim().split('\n');
      if (lines.length > 1) {
        const parts = lines[1].split(/\s+/);
        if (parts.length >= 5) {
          volumeInfo.backups = {
            path: "/mnt/backups",
            size: parts[1],
            used: parts[2],
            available: parts[3],
            usePercent: parts[4],
            mounted: true
          };
        }
      }
    } else {
      volumeInfo.backups = { path: "/mnt/backups", mounted: false };
    }
  } catch (err) {
    // Volume info is optional, don't fail the status endpoint
    console.error("Failed to get volume info:", err.message);
  }

  const status = {
    ready: dbReady,
    engine: "postgres",
    version: "16",
    uptime: process.uptime(),
    connections: {
      current: 0, // TODO: Query actual connection count
      max: parseInt(process.env.POSTGRES_MAX_CONNECTIONS || "100"),
    },
    volumes: volumeInfo
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
