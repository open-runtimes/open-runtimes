const express = require("express");
const { MongoClient } = require("mongodb");
const { spawn, execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const crypto = require("crypto");

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

// Replica set configuration
const REPLICA_SET_NAME = process.env.MONGO_REPLICA_SET_NAME || '';
const REPLICA_SET_KEY = process.env.MONGO_REPLICA_SET_KEY || '';
const REPLICA_SET_MEMBERS = process.env.MONGO_REPLICA_SET_MEMBERS || ''; // Format: host1:port1,host2:port2,...
const REPLICA_SET_PRIORITY = parseInt(process.env.MONGO_REPLICA_SET_PRIORITY || '1', 10);
const REPLICA_SET_VOTES = parseInt(process.env.MONGO_REPLICA_SET_VOTES || '1', 10);
const REPLICA_SET_HIDDEN = process.env.MONGO_REPLICA_SET_HIDDEN === 'true';
const REPLICA_SET_ARBITER = process.env.MONGO_REPLICA_SET_ARBITER === 'true';
const REPLICA_SET_DELAY = parseInt(process.env.MONGO_REPLICA_SET_DELAY || '0', 10); // Delayed replica in seconds
const REPLICA_SET_INIT = process.env.MONGO_REPLICA_SET_INIT === 'true';
const REPLICA_SET_MEMBER_ID = process.env.MONGO_REPLICA_SET_MEMBER_ID || ''; // This member's host:port
const OPLOG_SIZE_MB = parseInt(process.env.MONGO_OPLOG_SIZE_MB || '0', 10); // 0 = default
const READ_PREFERENCE = process.env.MONGO_READ_PREFERENCE || 'primary'; // primary, primaryPreferred, secondary, secondaryPreferred, nearest
const WRITE_CONCERN = process.env.MONGO_WRITE_CONCERN || 'majority';
const WRITE_CONCERN_TIMEOUT = parseInt(process.env.MONGO_WRITE_CONCERN_TIMEOUT || '10000', 10);

const KEYFILE_PATH = '/data/mongodb-keyfile';

let mongoProcess = null;
let dbReady = false;
let client = null;
let startupTimingWritten = false;
let replicaSetInitialized = false;

// Check if replica set mode is enabled
function isReplicaSetMode() {
  return !!REPLICA_SET_NAME;
}

// Generate or write keyfile for replica set authentication
function setupKeyfile() {
  if (!isReplicaSetMode()) {
    return;
  }

  console.log("Setting up replica set keyfile...");

  try {
    let keyContent = REPLICA_SET_KEY;

    // If no key provided, generate one (for testing only)
    if (!keyContent) {
      console.warn("WARNING: MONGO_REPLICA_SET_KEY not provided, generating random key (for testing only)");
      keyContent = crypto.randomBytes(756).toString('base64');
    }

    // Write keyfile
    fs.writeFileSync(KEYFILE_PATH, keyContent);
    fs.chmodSync(KEYFILE_PATH, 0o400);
    // MongoDB requires the keyfile to be owned by the mongodb user (uid 999)
    try {
      execSync(`chown 999:999 ${KEYFILE_PATH}`);
    } catch (e) {
      // May fail if not running as root, that's ok in some environments
      console.log("Could not chown keyfile (may not be running as root)");
    }

    console.log(`Keyfile written to ${KEYFILE_PATH}`);
  } catch (err) {
    console.error(`Failed to setup keyfile: ${err.message}`);
  }
}

// Initialize replica set
async function initializeReplicaSet() {
  if (!isReplicaSetMode() || !REPLICA_SET_INIT) {
    return;
  }

  console.log("Initializing replica set...");

  let retries = 30;
  while (retries > 0) {
    try {
      // Connect without authentication first (replica set not initialized yet)
      const tempUri = `mongodb://localhost:27017/?directConnection=true`;
      const tempClient = new MongoClient(tempUri);
      await tempClient.connect();

      const adminDb = tempClient.db("admin");

      // Check if already initialized
      try {
        const status = await adminDb.command({ replSetGetStatus: 1 });
        if (status.ok) {
          console.log("Replica set already initialized");
          await tempClient.close();
          replicaSetInitialized = true;
          return;
        }
      } catch (err) {
        // Expected error if not initialized: NotYetInitialized
        if (!err.message.includes("NotYetInitialized") && !err.message.includes("no replset config")) {
          console.log(`Replica set status check: ${err.message}`);
        }
      }

      // Parse members
      const members = [];
      if (REPLICA_SET_MEMBERS) {
        REPLICA_SET_MEMBERS.split(',').forEach((member, index) => {
          const [host, port] = member.trim().split(':');
          members.push({
            _id: index,
            host: port ? `${host}:${port}` : `${host}:27017`,
          });
        });
      } else if (REPLICA_SET_MEMBER_ID) {
        // Single member (this node)
        members.push({
          _id: 0,
          host: REPLICA_SET_MEMBER_ID,
          priority: REPLICA_SET_PRIORITY,
          votes: REPLICA_SET_VOTES,
          hidden: REPLICA_SET_HIDDEN,
          arbiterOnly: REPLICA_SET_ARBITER,
          ...(REPLICA_SET_DELAY > 0 ? { secondaryDelaySecs: REPLICA_SET_DELAY, priority: 0 } : {})
        });
      } else {
        // Default to localhost
        members.push({
          _id: 0,
          host: 'localhost:27017'
        });
      }

      const config = {
        _id: REPLICA_SET_NAME,
        members: members
      };

      console.log("Initializing replica set with config:", JSON.stringify(config, null, 2));

      await adminDb.command({ replSetInitiate: config });
      console.log("Replica set initialized successfully");

      // Wait for replica set to be ready
      let ready = false;
      let readyRetries = 30;
      while (!ready && readyRetries > 0) {
        try {
          const status = await adminDb.command({ replSetGetStatus: 1 });
          const self = status.members?.find(m => m.self);
          if (self && (self.stateStr === 'PRIMARY' || self.stateStr === 'SECONDARY')) {
            ready = true;
            console.log(`Replica set member is ${self.stateStr}`);
          }
        } catch (e) {
          // Ignore
        }
        if (!ready) {
          await new Promise(resolve => setTimeout(resolve, 2000));
          readyRetries--;
        }
      }

      await tempClient.close();
      replicaSetInitialized = true;
      return;
    } catch (err) {
      console.error(`Failed to initialize replica set (retries left: ${retries}):`, err.message);
      retries--;
      await new Promise(resolve => setTimeout(resolve, 2000));
    }
  }
}

// Add this node to existing replica set
async function addToReplicaSet() {
  if (!isReplicaSetMode() || REPLICA_SET_INIT || !REPLICA_SET_MEMBER_ID) {
    return;
  }

  // This node will be added by the primary when it connects
  // Just wait for the primary to add us
  console.log(`This node (${REPLICA_SET_MEMBER_ID}) is waiting to be added to replica set '${REPLICA_SET_NAME}'`);
}

function startMongoDB() {
  console.log("Starting MongoDB server...");

  const username = process.env.MONGO_INITDB_ROOT_USERNAME;
  const password = process.env.MONGO_INITDB_ROOT_PASSWORD;
  const database = process.env.MONGO_INITDB_DATABASE;

  const dbPath = "/data/db";
  const needsInit = !fs.existsSync(`${dbPath}/mongod.lock`);

  if (needsInit) {
    console.log("MongoDB not initialized, will create root user on first start...");
  }

  // Setup keyfile for replica set
  if (isReplicaSetMode()) {
    setupKeyfile();
  }

  const args = [
    "--bind_ip_all",
  ];

  // Add replica set configuration
  if (isReplicaSetMode()) {
    args.push("--replSet", REPLICA_SET_NAME);
    args.push("--keyFile", KEYFILE_PATH);

    if (OPLOG_SIZE_MB > 0) {
      args.push("--oplogSize", OPLOG_SIZE_MB.toString());
    }
  } else {
    // Only enable auth in standalone mode (replica set uses keyfile)
    args.push("--auth");
  }

  console.log(`Starting MongoDB with args: ${args.join(' ')}`);

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

        // Write startup timing if provided
        if (process.env.STARTUP_TIME && !startupTimingWritten) {
          const start = parseFloat(process.env.STARTUP_TIME);
          const end = parseFloat(fs.readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
          const elapsed = (end - start).toFixed(3);
          fs.appendFileSync('/mnt/telemetry/timings.txt', `startup=${elapsed}\n`);
          startupTimingWritten = true;
          console.log(`Recorded startup timing: ${elapsed}s`);
        }

        if (needsInit && !initAttempted) {
          initAttempted = true;
          setTimeout(async () => {
            await initializeRootUser(username, password, database);
            if (isReplicaSetMode()) {
              await initializeReplicaSet();
              await addToReplicaSet();
            }
            connectHealthClient();
          }, 2000);
        } else {
          setTimeout(async () => {
            if (isReplicaSetMode() && REPLICA_SET_INIT) {
              await initializeReplicaSet();
            }
            connectHealthClient();
          }, 2000);
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

    // For replica set, connect without auth initially
    const uri = isReplicaSetMode()
      ? "mongodb://localhost:27017/?directConnection=true"
      : "mongodb://localhost:27017/";
    const tempClient = new MongoClient(uri);
    await tempClient.connect();

    const adminDb = tempClient.db("admin");

    // Create root user
    try {
      await adminDb.command({
        createUser: username,
        pwd: password,
        roles: [{ role: "root", db: "admin" }]
      });
      console.log("Root user created successfully");
    } catch (err) {
      if (err.codeName === "DuplicateKey" || err.code === 51003) {
        console.log("Root user already exists, continuing...");
      } else {
        throw err;
      }
    }

    if (database && database !== "admin") {
      try {
        await tempClient.db(database).createCollection("_init");
        console.log(`Database '${database}' created`);
      } catch (e) {
        // Collection might already exist
      }
    }

    await tempClient.close();
  } catch (err) {
    console.error("Failed to initialize root user:", err.message);
    if (err.codeName === "DuplicateKey" || err.code === 51003) {
      console.log("Root user already exists, continuing...");
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

    let uri;
    if (isReplicaSetMode()) {
      uri = `mongodb://${username}:${password}@localhost:27017/?authSource=${authDb}&directConnection=true`;
    } else {
      uri = `mongodb://${username}:${password}@localhost:27017/?authSource=${authDb}`;
    }

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

// Get replica set status
async function getReplicaSetStatus() {
  if (!client || !isReplicaSetMode()) {
    return null;
  }

  try {
    const adminDb = client.db("admin");
    const status = await adminDb.command({ replSetGetStatus: 1 });

    const self = status.members?.find(m => m.self);

    return {
      set: status.set,
      myState: status.myState,
      stateStr: self?.stateStr || 'UNKNOWN',
      term: status.term,
      syncSourceHost: status.syncSourceHost || null,
      syncSourceId: status.syncSourceId,
      heartbeatIntervalMillis: status.heartbeatIntervalMillis,
      majorityVoteCount: status.majorityVoteCount,
      writeMajorityCount: status.writeMajorityCount,
      members: status.members?.map(m => ({
        id: m._id,
        name: m.name,
        health: m.health,
        state: m.state,
        stateStr: m.stateStr,
        uptime: m.uptime,
        optime: m.optime,
        optimeDate: m.optimeDate,
        lastHeartbeat: m.lastHeartbeat,
        lastHeartbeatRecv: m.lastHeartbeatRecv,
        pingMs: m.pingMs,
        syncSourceHost: m.syncSourceHost,
        syncSourceId: m.syncSourceId,
        configVersion: m.configVersion,
        configTerm: m.configTerm,
        self: m.self || false
      })) || [],
      optimes: {
        lastCommittedOpTime: status.optimes?.lastCommittedOpTime,
        lastCommittedWallTime: status.optimes?.lastCommittedWallTime,
        readConcernMajorityOpTime: status.optimes?.readConcernMajorityOpTime,
        appliedOpTime: status.optimes?.appliedOpTime,
        durableOpTime: status.optimes?.durableOpTime,
        lastAppliedWallTime: status.optimes?.lastAppliedWallTime,
        lastDurableWallTime: status.optimes?.lastDurableWallTime
      }
    };
  } catch (err) {
    console.error("Failed to get replica set status:", err.message);
    return { error: err.message };
  }
}

// Get replica set configuration
async function getReplicaSetConfig() {
  if (!client || !isReplicaSetMode()) {
    return null;
  }

  try {
    const adminDb = client.db("admin");
    const config = await adminDb.command({ replSetGetConfig: 1 });

    return {
      id: config.config._id,
      version: config.config.version,
      term: config.config.term,
      members: config.config.members?.map(m => ({
        id: m._id,
        host: m.host,
        arbiterOnly: m.arbiterOnly || false,
        hidden: m.hidden || false,
        priority: m.priority,
        votes: m.votes,
        secondaryDelaySecs: m.secondaryDelaySecs || 0,
        tags: m.tags || {}
      })) || [],
      settings: {
        chainingAllowed: config.config.settings?.chainingAllowed,
        heartbeatIntervalMillis: config.config.settings?.heartbeatIntervalMillis,
        heartbeatTimeoutSecs: config.config.settings?.heartbeatTimeoutSecs,
        electionTimeoutMillis: config.config.settings?.electionTimeoutMillis,
        catchUpTimeoutMillis: config.config.settings?.catchUpTimeoutMillis,
        getLastErrorModes: config.config.settings?.getLastErrorModes,
        getLastErrorDefaults: config.config.settings?.getLastErrorDefaults,
        replicaSetId: config.config.settings?.replicaSetId
      }
    };
  } catch (err) {
    console.error("Failed to get replica set config:", err.message);
    return { error: err.message };
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

  const replicaSetStatus = await getReplicaSetStatus();

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
    replication: replicaSetStatus ? {
      mode: 'replica_set',
      enabled: true,
      ...replicaSetStatus
    } : {
      mode: 'standalone',
      enabled: false
    }
  };

  res.json(status);
});

// Replication-specific endpoints
app.get("/__opr/replication", async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.json({ mode: 'standalone', enabled: false });
  }

  const status = await getReplicaSetStatus();
  const config = await getReplicaSetConfig();

  res.json({
    mode: 'replica_set',
    enabled: true,
    replicaSetName: REPLICA_SET_NAME,
    status: status,
    config: config
  });
});

// Step down primary (force election)
app.post("/__opr/replication/stepdown", async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");

    // Check if this is the primary
    const status = await adminDb.command({ replSetGetStatus: 1 });
    const self = status.members?.find(m => m.self);

    if (self?.stateStr !== 'PRIMARY') {
      return res.status(400).json({ error: 'This node is not the primary' });
    }

    // Step down (default 60 seconds, can be overridden)
    const stepDownSecs = parseInt(req.query.seconds || '60', 10);
    await adminDb.command({ replSetStepDown: stepDownSecs });

    res.json({ success: true, message: `Primary stepped down for ${stepDownSecs} seconds` });
  } catch (err) {
    // StepDown will close connections, which throws an error
    if (err.message.includes('network') || err.message.includes('connection')) {
      res.json({ success: true, message: 'Primary stepped down' });
    } else {
      res.status(500).json({ error: err.message });
    }
  }
});

// Freeze a secondary (prevent it from becoming primary)
app.post("/__opr/replication/freeze", async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");
    const freezeSecs = parseInt(req.query.seconds || '60', 10);

    await adminDb.command({ replSetFreeze: freezeSecs });
    res.json({ success: true, message: `Node frozen for ${freezeSecs} seconds` });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Add a new member to the replica set
app.post("/__opr/replication/member", express.json(), async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");

    // Check if this is the primary
    const status = await adminDb.command({ replSetGetStatus: 1 });
    const self = status.members?.find(m => m.self);

    if (self?.stateStr !== 'PRIMARY') {
      return res.status(400).json({ error: 'Members can only be added from the primary' });
    }

    const { host, priority = 1, votes = 1, hidden = false, arbiterOnly = false, secondaryDelaySecs = 0 } = req.body;

    if (!host) {
      return res.status(400).json({ error: 'host is required' });
    }

    // Get current config
    const configRes = await adminDb.command({ replSetGetConfig: 1 });
    const config = configRes.config;

    // Determine next _id
    const maxId = Math.max(...config.members.map(m => m._id));

    // Add new member
    const newMember = {
      _id: maxId + 1,
      host: host,
      priority: hidden || secondaryDelaySecs > 0 ? 0 : priority,
      votes: votes,
      hidden: hidden,
      arbiterOnly: arbiterOnly,
      secondaryDelaySecs: secondaryDelaySecs
    };

    config.members.push(newMember);
    config.version++;

    await adminDb.command({ replSetReconfig: config });
    res.json({ success: true, message: `Member '${host}' added to replica set`, memberId: newMember._id });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Remove a member from the replica set
app.delete("/__opr/replication/member/:host", async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");

    // Check if this is the primary
    const status = await adminDb.command({ replSetGetStatus: 1 });
    const self = status.members?.find(m => m.self);

    if (self?.stateStr !== 'PRIMARY') {
      return res.status(400).json({ error: 'Members can only be removed from the primary' });
    }

    const hostToRemove = req.params.host;

    // Get current config
    const configRes = await adminDb.command({ replSetGetConfig: 1 });
    const config = configRes.config;

    // Find and remove the member
    const memberIndex = config.members.findIndex(m => m.host === hostToRemove);
    if (memberIndex === -1) {
      return res.status(404).json({ error: `Member '${hostToRemove}' not found in replica set` });
    }

    config.members.splice(memberIndex, 1);
    config.version++;

    await adminDb.command({ replSetReconfig: config });
    res.json({ success: true, message: `Member '${hostToRemove}' removed from replica set` });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Reconfigure a member's settings
app.put("/__opr/replication/member/:host", express.json(), async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");

    // Check if this is the primary
    const status = await adminDb.command({ replSetGetStatus: 1 });
    const self = status.members?.find(m => m.self);

    if (self?.stateStr !== 'PRIMARY') {
      return res.status(400).json({ error: 'Members can only be reconfigured from the primary' });
    }

    const hostToUpdate = req.params.host;
    const { priority, votes, hidden, secondaryDelaySecs } = req.body;

    // Get current config
    const configRes = await adminDb.command({ replSetGetConfig: 1 });
    const config = configRes.config;

    // Find the member
    const member = config.members.find(m => m.host === hostToUpdate);
    if (!member) {
      return res.status(404).json({ error: `Member '${hostToUpdate}' not found in replica set` });
    }

    // Update fields
    if (priority !== undefined) member.priority = priority;
    if (votes !== undefined) member.votes = votes;
    if (hidden !== undefined) member.hidden = hidden;
    if (secondaryDelaySecs !== undefined) member.secondaryDelaySecs = secondaryDelaySecs;

    config.version++;

    await adminDb.command({ replSetReconfig: config });
    res.json({ success: true, message: `Member '${hostToUpdate}' reconfigured` });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Force reconfiguration (dangerous - use only in emergencies)
app.post("/__opr/replication/force-reconfig", express.json(), async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const adminDb = client.db("admin");
    const { config } = req.body;

    if (!config) {
      return res.status(400).json({ error: 'config object is required' });
    }

    await adminDb.command({ replSetReconfig: config, force: true });
    res.json({ success: true, message: 'Replica set forcefully reconfigured' });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// Get oplog info
app.get("/__opr/replication/oplog", async (req, res) => {
  if (!isReplicaSetMode()) {
    return res.status(400).json({ error: 'Only available in replica set mode' });
  }

  try {
    const localDb = client.db("local");

    // Get oplog stats
    const stats = await localDb.collection("oplog.rs").stats();

    // Get oplog time range
    const first = await localDb.collection("oplog.rs").findOne({}, { sort: { $natural: 1 } });
    const last = await localDb.collection("oplog.rs").findOne({}, { sort: { $natural: -1 } });

    let oplogWindowHours = null;
    if (first?.ts && last?.ts) {
      const firstTs = first.ts.getHighBits();
      const lastTs = last.ts.getHighBits();
      oplogWindowHours = ((lastTs - firstTs) / 3600).toFixed(2);
    }

    res.json({
      storageSize: stats.storageSize,
      size: stats.size,
      count: stats.count,
      avgObjSize: stats.avgObjSize,
      capped: stats.capped,
      maxSize: stats.maxSize,
      firstTimestamp: first?.ts || null,
      lastTimestamp: last?.ts || null,
      oplogWindowHours: oplogWindowHours
    });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
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
