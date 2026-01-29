const express = require("express");
const mysql = require("mysql2/promise");
const { spawn, execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const app = express();
const MANAGEMENT_PORT = 3000;

if (!process.env.MYSQL_ROOT_PASSWORD) {
  console.warn("WARNING: MYSQL_ROOT_PASSWORD not set, using default 'mysql' (for testing only)");
  process.env.MYSQL_ROOT_PASSWORD = 'mysql';
}
if (!process.env.MYSQL_DATABASE) {
  console.warn("WARNING: MYSQL_DATABASE not set, using default 'mysql' (for testing only)");
  process.env.MYSQL_DATABASE = 'mysql';
}

// Replication configuration
const REPLICATION_MODE = process.env.MYSQL_REPLICATION_MODE || 'standalone'; // standalone, primary, replica
const SERVER_ID = parseInt(process.env.MYSQL_SERVER_ID || '1', 10);
const REPLICATION_USER = process.env.MYSQL_REPLICATION_USER || 'replicator';
const REPLICATION_PASSWORD = process.env.MYSQL_REPLICATION_PASSWORD || '';
const PRIMARY_HOST = process.env.MYSQL_PRIMARY_HOST || '';
const PRIMARY_PORT = parseInt(process.env.MYSQL_PRIMARY_PORT || '3306', 10);
const GTID_MODE = process.env.MYSQL_GTID_MODE || 'ON';
const ENFORCE_GTID_CONSISTENCY = process.env.MYSQL_ENFORCE_GTID_CONSISTENCY || 'ON';
const BINLOG_FORMAT = process.env.MYSQL_BINLOG_FORMAT || 'ROW';
const BINLOG_ROW_IMAGE = process.env.MYSQL_BINLOG_ROW_IMAGE || 'FULL';
const SYNC_BINLOG = parseInt(process.env.MYSQL_SYNC_BINLOG || '1', 10);
const RELAY_LOG_RECOVERY = process.env.MYSQL_RELAY_LOG_RECOVERY || 'ON';
const READ_ONLY = process.env.MYSQL_READ_ONLY || (REPLICATION_MODE === 'replica' ? 'ON' : 'OFF');
const SUPER_READ_ONLY = process.env.MYSQL_SUPER_READ_ONLY || (REPLICATION_MODE === 'replica' ? 'ON' : 'OFF');
const REPLICATE_DO_DB = process.env.MYSQL_REPLICATE_DO_DB || '';
const REPLICATE_IGNORE_DB = process.env.MYSQL_REPLICATE_IGNORE_DB || '';
const REPLICATION_RETRY_INTERVAL = parseInt(process.env.MYSQL_REPLICATION_RETRY_INTERVAL || '60', 10);
const REPLICATION_CONNECT_RETRY = parseInt(process.env.MYSQL_REPLICATION_CONNECT_RETRY || '60', 10);
const SEMI_SYNC_ENABLED = process.env.MYSQL_SEMI_SYNC_ENABLED === 'true';
const SEMI_SYNC_TIMEOUT = parseInt(process.env.MYSQL_SEMI_SYNC_TIMEOUT || '10000', 10);

let mysqlProcess = null;
let dbReady = false;
let connection = null;
let startupTimingWritten = false;
let replicationConfigured = false;

// Generate MySQL configuration file for replication
function generateReplicationConfig() {
  if (REPLICATION_MODE === 'standalone') {
    console.log("Replication mode: standalone - no replication configuration needed");
    return;
  }

  console.log(`Configuring MySQL for replication mode: ${REPLICATION_MODE}`);

  let config = `[mysqld]
# Replication configuration
server-id=${SERVER_ID}
log-bin=mysql-bin
binlog-format=${BINLOG_FORMAT}
binlog-row-image=${BINLOG_ROW_IMAGE}
sync-binlog=${SYNC_BINLOG}
`;

  // GTID configuration
  if (GTID_MODE === 'ON') {
    config += `
# GTID configuration
gtid-mode=${GTID_MODE}
enforce-gtid-consistency=${ENFORCE_GTID_CONSISTENCY}
`;
  }

  if (REPLICATION_MODE === 'primary') {
    config += `
# Primary-specific settings
log-slave-updates=ON
`;
  } else if (REPLICATION_MODE === 'replica') {
    config += `
# Replica-specific settings
relay-log=mysql-relay-bin
relay-log-recovery=${RELAY_LOG_RECOVERY}
read-only=${READ_ONLY}
super-read-only=${SUPER_READ_ONLY}
log-slave-updates=ON
skip-slave-start=ON
`;
    // Database filtering
    if (REPLICATE_DO_DB) {
      REPLICATE_DO_DB.split(',').forEach(db => {
        config += `replicate-do-db=${db.trim()}\n`;
      });
    }
    if (REPLICATE_IGNORE_DB) {
      REPLICATE_IGNORE_DB.split(',').forEach(db => {
        config += `replicate-ignore-db=${db.trim()}\n`;
      });
    }
  }

  // Write configuration file
  const configDir = '/etc/mysql/conf.d';
  const configPath = path.join(configDir, 'replication.cnf');

  try {
    if (!fs.existsSync(configDir)) {
      fs.mkdirSync(configDir, { recursive: true });
    }
    fs.writeFileSync(configPath, config);
    console.log(`Replication configuration written to ${configPath}`);
  } catch (err) {
    console.error(`Failed to write replication config: ${err.message}`);
  }
}

// Create replication user on primary
async function createReplicationUser() {
  if (REPLICATION_MODE !== 'primary' || !REPLICATION_PASSWORD) {
    return;
  }

  console.log("Creating replication user...");

  let retries = 10;
  while (retries > 0) {
    try {
      const conn = await mysql.createConnection({
        host: "localhost",
        port: 3306,
        user: "root",
        password: process.env.MYSQL_ROOT_PASSWORD,
      });

      // Create replication user
      await conn.query(`CREATE USER IF NOT EXISTS '${REPLICATION_USER}'@'%' IDENTIFIED BY '${REPLICATION_PASSWORD}'`);
      await conn.query(`GRANT REPLICATION SLAVE, REPLICATION CLIENT ON *.* TO '${REPLICATION_USER}'@'%'`);
      await conn.query(`FLUSH PRIVILEGES`);

      // Enable semi-sync if configured
      if (SEMI_SYNC_ENABLED) {
        try {
          await conn.query(`INSTALL PLUGIN rpl_semi_sync_source SONAME 'semisync_source.so'`);
          await conn.query(`SET GLOBAL rpl_semi_sync_source_enabled = 1`);
          await conn.query(`SET GLOBAL rpl_semi_sync_source_timeout = ${SEMI_SYNC_TIMEOUT}`);
          console.log("Semi-synchronous replication enabled on primary");
        } catch (err) {
          if (!err.message.includes('already exists')) {
            console.error("Failed to enable semi-sync:", err.message);
          }
        }
      }

      await conn.end();
      console.log(`Replication user '${REPLICATION_USER}' created successfully`);
      return;
    } catch (err) {
      console.error(`Failed to create replication user (retries left: ${retries}):`, err.message);
      retries--;
      await new Promise(resolve => setTimeout(resolve, 3000));
    }
  }
}

// Configure replica to connect to primary
async function configureReplica() {
  if (REPLICATION_MODE !== 'replica' || !PRIMARY_HOST || !REPLICATION_PASSWORD) {
    console.log("Skipping replica configuration - missing required parameters");
    return;
  }

  console.log(`Configuring replica to connect to primary: ${PRIMARY_HOST}:${PRIMARY_PORT}`);

  let retries = 10;
  while (retries > 0) {
    try {
      const conn = await mysql.createConnection({
        host: "localhost",
        port: 3306,
        user: "root",
        password: process.env.MYSQL_ROOT_PASSWORD,
      });

      // Stop any existing replication
      try {
        await conn.query('STOP REPLICA');
      } catch (e) {
        // Ignore if replication not running
      }

      // Configure replication source
      if (GTID_MODE === 'ON') {
        // GTID-based replication
        await conn.query(`
          CHANGE REPLICATION SOURCE TO
            SOURCE_HOST='${PRIMARY_HOST}',
            SOURCE_PORT=${PRIMARY_PORT},
            SOURCE_USER='${REPLICATION_USER}',
            SOURCE_PASSWORD='${REPLICATION_PASSWORD}',
            SOURCE_AUTO_POSITION=1,
            SOURCE_CONNECT_RETRY=${REPLICATION_CONNECT_RETRY},
            SOURCE_RETRY_COUNT=86400
        `);
      } else {
        // Position-based replication
        await conn.query(`
          CHANGE REPLICATION SOURCE TO
            SOURCE_HOST='${PRIMARY_HOST}',
            SOURCE_PORT=${PRIMARY_PORT},
            SOURCE_USER='${REPLICATION_USER}',
            SOURCE_PASSWORD='${REPLICATION_PASSWORD}',
            SOURCE_CONNECT_RETRY=${REPLICATION_CONNECT_RETRY},
            SOURCE_RETRY_COUNT=86400
        `);
      }

      // Enable semi-sync if configured
      if (SEMI_SYNC_ENABLED) {
        try {
          await conn.query(`INSTALL PLUGIN rpl_semi_sync_replica SONAME 'semisync_replica.so'`);
          await conn.query(`SET GLOBAL rpl_semi_sync_replica_enabled = 1`);
          console.log("Semi-synchronous replication enabled on replica");
        } catch (err) {
          if (!err.message.includes('already exists')) {
            console.error("Failed to enable semi-sync:", err.message);
          }
        }
      }

      // Start replication
      await conn.query('START REPLICA');

      // Verify replication is working
      const [status] = await conn.query('SHOW REPLICA STATUS');
      if (status && status.length > 0) {
        const replicaStatus = status[0];
        console.log(`Replica IO Running: ${replicaStatus.Replica_IO_Running}`);
        console.log(`Replica SQL Running: ${replicaStatus.Replica_SQL_Running}`);

        if (replicaStatus.Replica_IO_Running === 'Yes' && replicaStatus.Replica_SQL_Running === 'Yes') {
          console.log("Replication configured and running successfully");
          replicationConfigured = true;
        } else if (replicaStatus.Last_IO_Error || replicaStatus.Last_SQL_Error) {
          console.error(`Replication error - IO: ${replicaStatus.Last_IO_Error}, SQL: ${replicaStatus.Last_SQL_Error}`);
        }
      }

      await conn.end();
      return;
    } catch (err) {
      console.error(`Failed to configure replica (retries left: ${retries}):`, err.message);
      retries--;
      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  }
}

// Start MySQL server
function startMySQL() {
  console.log("Starting MySQL server...");

  // Generate replication config before starting MySQL
  generateReplicationConfig();

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

        // Write startup timing if provided
        if (process.env.STARTUP_TIME && !startupTimingWritten) {
          const start = parseFloat(process.env.STARTUP_TIME);
          const end = parseFloat(fs.readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
          const elapsed = (end - start).toFixed(3);
          fs.appendFileSync('/mnt/telemetry/timings.txt', `startup=${elapsed}\n`);
          startupTimingWritten = true;
          console.log(`Recorded startup timing: ${elapsed}s`);
        }

        // Configure replication after MySQL is ready
        setTimeout(async () => {
          if (REPLICATION_MODE === 'primary') {
            await createReplicationUser();
          } else if (REPLICATION_MODE === 'replica') {
            await configureReplica();
          }
          connectHealthClient();
        }, 2000);
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

        // Write startup timing if provided
        if (process.env.STARTUP_TIME && !startupTimingWritten) {
          const start = parseFloat(process.env.STARTUP_TIME);
          const end = parseFloat(fs.readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
          const elapsed = (end - start).toFixed(3);
          fs.appendFileSync('/mnt/telemetry/timings.txt', `startup=${elapsed}\n`);
          startupTimingWritten = true;
          console.log(`Recorded startup timing: ${elapsed}s`);
        }

        // Configure replication after MySQL is ready
        setTimeout(async () => {
          if (REPLICATION_MODE === 'primary') {
            await createReplicationUser();
          } else if (REPLICATION_MODE === 'replica') {
            await configureReplica();
          }
          connectHealthClient();
        }, 2000);
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

// Get volume information
function getVolumeInfo() {
  const volumes = {};

  // Data volume
  const dataPath = '/var/lib/mysql';
  try {
    const dfOutput = execSync(`df -B1 ${dataPath} 2>/dev/null | tail -1`, { encoding: 'utf8' });
    const parts = dfOutput.trim().split(/\s+/);
    if (parts.length >= 4) {
      const total = parseInt(parts[1]);
      const used = parseInt(parts[2]);
      const available = parseInt(parts[3]);
      const usedPercent = total > 0 ? Math.round((used / total) * 100) : 0;
      const availableGB = (available / (1024 * 1024 * 1024)).toFixed(1);

      volumes.data = {
        path: dataPath,
        usedPercent: `${usedPercent}%`,
        available: `${availableGB}GB`,
        mounted: true
      };
    }
  } catch (err) {
    volumes.data = { path: dataPath, usedPercent: '0%', available: '0GB', mounted: fs.existsSync(dataPath) };
  }

  // Backups volume (if exists)
  const backupsPath = '/mnt/backups';
  if (fs.existsSync(backupsPath)) {
    try {
      const dfOutput = execSync(`df -B1 ${backupsPath} 2>/dev/null | tail -1`, { encoding: 'utf8' });
      const parts = dfOutput.trim().split(/\s+/);
      if (parts.length >= 4) {
        const total = parseInt(parts[1]);
        const used = parseInt(parts[2]);
        const available = parseInt(parts[3]);
        const usedPercent = total > 0 ? Math.round((used / total) * 100) : 0;
        const availableGB = (available / (1024 * 1024 * 1024)).toFixed(1);

        volumes.backups = {
          path: backupsPath,
          usedPercent: `${usedPercent}%`,
          available: `${availableGB}GB`,
          mounted: true
        };
      }
    } catch (err) {
      volumes.backups = { path: backupsPath, usedPercent: '0%', available: '0GB', mounted: true };
    }
  }

  return volumes;
}

// Get replication status
async function getReplicationStatus() {
  if (!connection || REPLICATION_MODE === 'standalone') {
    return null;
  }

  try {
    if (REPLICATION_MODE === 'primary') {
      const [status] = await connection.query('SHOW MASTER STATUS');
      const [replicas] = await connection.query('SHOW REPLICAS');

      let semiSyncStatus = null;
      if (SEMI_SYNC_ENABLED) {
        try {
          const [semiSync] = await connection.query("SHOW STATUS LIKE 'Rpl_semi_sync_source%'");
          semiSyncStatus = {};
          semiSync.forEach(row => {
            semiSyncStatus[row.Variable_name] = row.Value;
          });
        } catch (e) {}
      }

      return {
        role: 'primary',
        file: status[0]?.File || null,
        position: status[0]?.Position || null,
        binlogDoDb: status[0]?.Binlog_Do_DB || null,
        binlogIgnoreDb: status[0]?.Binlog_Ignore_DB || null,
        executedGtidSet: status[0]?.Executed_Gtid_Set || null,
        connectedReplicas: replicas?.length || 0,
        replicas: replicas?.map(r => ({
          serverId: r.Server_Id,
          host: r.Host,
          port: r.Port,
          sourceId: r.Source_Id
        })) || [],
        semiSync: semiSyncStatus
      };
    } else if (REPLICATION_MODE === 'replica') {
      const [status] = await connection.query('SHOW REPLICA STATUS');

      if (!status || status.length === 0) {
        return { role: 'replica', configured: false };
      }

      const s = status[0];
      let semiSyncStatus = null;
      if (SEMI_SYNC_ENABLED) {
        try {
          const [semiSync] = await connection.query("SHOW STATUS LIKE 'Rpl_semi_sync_replica%'");
          semiSyncStatus = {};
          semiSync.forEach(row => {
            semiSyncStatus[row.Variable_name] = row.Value;
          });
        } catch (e) {}
      }

      return {
        role: 'replica',
        configured: true,
        sourceHost: s.Source_Host,
        sourcePort: s.Source_Port,
        sourceUser: s.Source_User,
        ioRunning: s.Replica_IO_Running,
        sqlRunning: s.Replica_SQL_Running,
        lastIoError: s.Last_IO_Error || null,
        lastSqlError: s.Last_SQL_Error || null,
        secondsBehindSource: s.Seconds_Behind_Source,
        sourceLogFile: s.Source_Log_File,
        readSourceLogPos: s.Read_Source_Log_Pos,
        relayLogFile: s.Relay_Log_File,
        relayLogPos: s.Relay_Log_Pos,
        execSourceLogPos: s.Exec_Source_Log_Pos,
        retrievedGtidSet: s.Retrieved_Gtid_Set || null,
        executedGtidSet: s.Executed_Gtid_Set || null,
        semiSync: semiSyncStatus
      };
    }
  } catch (err) {
    console.error("Failed to get replication status:", err.message);
    return { error: err.message };
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

  const replicationStatus = await getReplicationStatus();
  const volumes = getVolumeInfo();

  const status = {
    ready: dbReady,
    engine: "mariadb",
    version: "11.4",
    uptime: process.uptime(),
    connections: {
      current: currentConnections,
      max: parseInt(process.env.MYSQL_MAX_CONNECTIONS || "151"),
    },
    volumes: volumes,
    replication: replicationStatus || {
      mode: REPLICATION_MODE,
      serverId: SERVER_ID
    }
  };

  res.json(status);
});

// Replication-specific endpoints
app.get("/__opr/replication", async (req, res) => {
  const status = await getReplicationStatus();
  if (!status) {
    return res.json({ mode: 'standalone', enabled: false });
  }
  res.json({
    mode: REPLICATION_MODE,
    enabled: REPLICATION_MODE !== 'standalone',
    serverId: SERVER_ID,
    gtidMode: GTID_MODE,
    ...status
  });
});

app.post("/__opr/replication/start", async (req, res) => {
  if (REPLICATION_MODE !== 'replica') {
    return res.status(400).json({ error: 'Only available on replica' });
  }

  try {
    await connection.query('START REPLICA');
    res.json({ success: true, message: 'Replication started' });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post("/__opr/replication/stop", async (req, res) => {
  if (REPLICATION_MODE !== 'replica') {
    return res.status(400).json({ error: 'Only available on replica' });
  }

  try {
    await connection.query('STOP REPLICA');
    res.json({ success: true, message: 'Replication stopped' });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.post("/__opr/replication/reset", async (req, res) => {
  if (REPLICATION_MODE !== 'replica') {
    return res.status(400).json({ error: 'Only available on replica' });
  }

  try {
    await connection.query('STOP REPLICA');
    await connection.query('RESET REPLICA ALL');
    res.json({ success: true, message: 'Replication reset' });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Promote replica to primary (for failover)
app.post("/__opr/replication/promote", async (req, res) => {
  if (REPLICATION_MODE !== 'replica') {
    return res.status(400).json({ error: 'Not a replica - cannot promote' });
  }

  try {
    console.log("[Failover] Promoting replica to primary...");

    // Stop replication
    await connection.query('STOP REPLICA');
    console.log("[Failover] Stopped replication");

    // Clear replication configuration
    await connection.query('RESET REPLICA ALL');
    console.log("[Failover] Cleared replication configuration");

    // Disable read-only mode to allow writes
    await connection.query('SET GLOBAL read_only = OFF');
    await connection.query('SET GLOBAL super_read_only = OFF');
    console.log("[Failover] Disabled read-only mode");

    // If semi-sync was enabled, reconfigure as source
    if (SEMI_SYNC_ENABLED) {
      try {
        await connection.query(`INSTALL PLUGIN rpl_semi_sync_source SONAME 'semisync_source.so'`);
        await connection.query(`SET GLOBAL rpl_semi_sync_source_enabled = 1`);
        await connection.query(`SET GLOBAL rpl_semi_sync_source_timeout = ${SEMI_SYNC_TIMEOUT}`);
        console.log("[Failover] Reconfigured semi-sync as source");
      } catch (err) {
        if (!err.message.includes('already exists')) {
          console.error("[Failover] Failed to configure semi-sync as source:", err.message);
        }
      }
    }

    console.log("[Failover] Promotion complete - this node is now the primary");

    res.json({
      success: true,
      message: 'Promoted to primary',
      timestamp: new Date().toISOString()
    });
  } catch (err) {
    console.error("[Failover] Promotion failed:", err.message);
    res.status(500).json({ error: err.message });
  }
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
