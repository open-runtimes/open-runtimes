# MongoDB Runtime

OpenRuntimes-based MongoDB database runtime for Appwrite dedicated databases.

## Overview

This runtime provides a MongoDB database engine wrapped in an OpenRuntimes management layer. It enables:

- **Cold-start support** for shared tier databases
- **Always-on deployment** for dedicated tier databases
- **Health check endpoints** for container orchestration
- **Telemetry and monitoring** via OpenRuntimes management API
- **Replica set support** for high availability

## Architecture

The runtime consists of two main components:

1. **MongoDB Engine**: Native MongoDB server running on port 27017
2. **OpenRuntimes Management Layer**: Node.js server on port 3000 providing:
   - Health checks (`/__opr/health`)
   - Status monitoring (`/__opr/status`)
   - Telemetry (`/__opr/timings`)
   - Replica set management (`/__opr/replication`)

## Available Versions

- MongoDB 8

## Usage

### Building the Runtime

```bash
docker build -t appwrite/mongodb:8-runtime runtimes/mongodb/versions/8/
```

### Running the Container

**⚠️ IMPORTANT: Credentials must be provided at runtime and should NEVER be hardcoded.**

```bash
# Generate secure password
MONGO_INITDB_ROOT_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  -p 27017:27017 \
  -p 3000:3000 \
  -e MONGO_INITDB_ROOT_USERNAME=admin \
  -e MONGO_INITDB_ROOT_PASSWORD="$MONGO_INITDB_ROOT_PASSWORD" \
  -e MONGO_INITDB_DATABASE=admin \
  -v mongodata:/data/db \
  appwrite/mongodb:8-runtime
```

### Environment Variables

#### Basic Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MONGO_INITDB_ROOT_USERNAME` | Root username | **YES** | (none - must provide) |
| `MONGO_INITDB_ROOT_PASSWORD` | Root password | **YES** | (none - must provide) |
| `MONGO_INITDB_DATABASE` | Authentication database | **YES** | (none - must provide) |

#### Replica Set Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MONGO_REPLICA_SET_NAME` | Replica set name (enables replica set mode) | No | (none - standalone) |
| `MONGO_REPLICA_SET_KEY` | Shared keyfile content for inter-member authentication | For replica sets | (auto-generated) |
| `MONGO_REPLICA_SET_MEMBERS` | Comma-separated list of members (host:port) | No | (single member) |
| `MONGO_REPLICA_SET_INIT` | Set to `true` on the primary to initialize replica set | No | `false` |
| `MONGO_REPLICA_SET_MEMBER_ID` | This member's host:port identifier | For members | (none) |
| `MONGO_REPLICA_SET_PRIORITY` | Election priority (0-100, 0 = never primary) | No | `1` |
| `MONGO_REPLICA_SET_VOTES` | Number of votes in elections | No | `1` |
| `MONGO_REPLICA_SET_HIDDEN` | Hide this member from client discovery | No | `false` |
| `MONGO_REPLICA_SET_ARBITER` | Configure as arbiter (voting only) | No | `false` |
| `MONGO_REPLICA_SET_DELAY` | Seconds to delay replication (for delayed replica) | No | `0` |
| `MONGO_OPLOG_SIZE_MB` | Oplog size in megabytes | No | (MongoDB default) |
| `MONGO_READ_PREFERENCE` | Read preference: `primary`, `primaryPreferred`, `secondary`, `secondaryPreferred`, `nearest` | No | `primary` |
| `MONGO_WRITE_CONCERN` | Write concern level | No | `majority` |
| `MONGO_WRITE_CONCERN_TIMEOUT` | Write concern timeout in ms | No | `10000` |

**Security Notes:**
- The container will **fail to start** if required environment variables are not provided
- All database configuration must be provided at runtime (stateless design)
- Passwords should be generated securely (32+ characters, cryptographically random)
- In production, credentials are injected via Kubernetes Secrets
- Never commit credentials to version control
- For replica sets, all members must use the same keyfile content

### Health Check

```bash
curl http://localhost:3000/__opr/health
# Response: OK (200) if healthy, 503 if not ready
```

### Status Check

```bash
curl http://localhost:3000/__opr/status
# Returns JSON with database status, connections, uptime, and replica set info
```

## Replica Sets

### Setting Up a Replica Set

MongoDB replica sets provide automatic failover and data redundancy. A typical setup requires an odd number of voting members (3, 5, or 7).

#### Generate Shared Keyfile

All members must share the same keyfile for internal authentication:

```bash
MONGO_REPLICA_SET_KEY=$(openssl rand -base64 756)
```

#### Primary Member (Initial Setup)

```bash
MONGO_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  --name mongo-primary \
  -p 27017:27017 \
  -p 3000:3000 \
  -e MONGO_INITDB_ROOT_USERNAME=admin \
  -e MONGO_INITDB_ROOT_PASSWORD="$MONGO_PASSWORD" \
  -e MONGO_INITDB_DATABASE=admin \
  -e MONGO_REPLICA_SET_NAME=rs0 \
  -e MONGO_REPLICA_SET_KEY="$MONGO_REPLICA_SET_KEY" \
  -e MONGO_REPLICA_SET_INIT=true \
  -e MONGO_REPLICA_SET_MEMBER_ID=mongo-primary:27017 \
  -v mongo-primary-data:/data/db \
  appwrite/mongodb:8-runtime
```

#### Secondary Members

```bash
# Secondary 1
docker run -d \
  --name mongo-secondary1 \
  -p 27018:27017 \
  -p 3001:3000 \
  -e MONGO_INITDB_ROOT_USERNAME=admin \
  -e MONGO_INITDB_ROOT_PASSWORD="$MONGO_PASSWORD" \
  -e MONGO_INITDB_DATABASE=admin \
  -e MONGO_REPLICA_SET_NAME=rs0 \
  -e MONGO_REPLICA_SET_KEY="$MONGO_REPLICA_SET_KEY" \
  -e MONGO_REPLICA_SET_MEMBER_ID=mongo-secondary1:27017 \
  -v mongo-secondary1-data:/data/db \
  appwrite/mongodb:8-runtime

# Secondary 2
docker run -d \
  --name mongo-secondary2 \
  -p 27019:27017 \
  -p 3002:3000 \
  -e MONGO_INITDB_ROOT_USERNAME=admin \
  -e MONGO_INITDB_ROOT_PASSWORD="$MONGO_PASSWORD" \
  -e MONGO_INITDB_DATABASE=admin \
  -e MONGO_REPLICA_SET_NAME=rs0 \
  -e MONGO_REPLICA_SET_KEY="$MONGO_REPLICA_SET_KEY" \
  -e MONGO_REPLICA_SET_MEMBER_ID=mongo-secondary2:27017 \
  -v mongo-secondary2-data:/data/db \
  appwrite/mongodb:8-runtime
```

#### Add Secondaries to Replica Set

Use the primary's management API to add members:

```bash
# Add secondary 1
curl -X POST http://localhost:3000/__opr/replication/member \
  -H "Content-Type: application/json" \
  -d '{"host": "mongo-secondary1:27017"}'

# Add secondary 2
curl -X POST http://localhost:3000/__opr/replication/member \
  -H "Content-Type: application/json" \
  -d '{"host": "mongo-secondary2:27017"}'
```

### Replica Set Endpoints

#### Get Replica Set Status

```bash
curl http://localhost:3000/__opr/replication
```

Returns:
- Replica set configuration
- Member states (PRIMARY, SECONDARY, ARBITER, etc.)
- Replication lag information
- Election term and optime details

#### Step Down Primary

Force an election by stepping down the primary:

```bash
curl -X POST "http://localhost:3000/__opr/replication/stepdown?seconds=60"
```

The server will not accept election for the specified duration.

#### Freeze a Secondary

Prevent a secondary from becoming primary:

```bash
curl -X POST "http://localhost:3000/__opr/replication/freeze?seconds=120"
```

#### Add a Member

```bash
curl -X POST http://localhost:3000/__opr/replication/member \
  -H "Content-Type: application/json" \
  -d '{
    "host": "mongo-new:27017",
    "priority": 1,
    "votes": 1,
    "hidden": false
  }'
```

#### Remove a Member

```bash
curl -X DELETE "http://localhost:3000/__opr/replication/member/mongo-old:27017"
```

#### Reconfigure a Member

```bash
curl -X PUT "http://localhost:3000/__opr/replication/member/mongo-secondary1:27017" \
  -H "Content-Type: application/json" \
  -d '{
    "priority": 2,
    "votes": 1
  }'
```

#### Get Oplog Information

```bash
curl http://localhost:3000/__opr/replication/oplog
```

Returns oplog size, count, and time window information.

#### Force Reconfiguration (Emergency)

For emergency situations where the replica set is in a broken state:

```bash
curl -X POST http://localhost:3000/__opr/replication/force-reconfig \
  -H "Content-Type: application/json" \
  -d '{"config": {...}}'
```

### Special Member Types

#### Arbiter

An arbiter participates in elections but holds no data:

```bash
-e MONGO_REPLICA_SET_ARBITER=true \
-e MONGO_REPLICA_SET_PRIORITY=0
```

#### Hidden Member

Hidden members are invisible to clients but participate in replication:

```bash
-e MONGO_REPLICA_SET_HIDDEN=true \
-e MONGO_REPLICA_SET_PRIORITY=0
```

#### Delayed Replica

A delayed replica stays behind by a specified time (useful for recovery from human errors):

```bash
-e MONGO_REPLICA_SET_DELAY=3600  # 1 hour delay
-e MONGO_REPLICA_SET_PRIORITY=0
```

## Integration with Appwrite

This runtime is designed to be used with Appwrite's dedicated database infrastructure:

- **Shared Tier**: Automatic cold-start and pause lifecycle
- **Dedicated Tier**: Always-on deployment with guaranteed resources
- **TCP Proxy**: Routes connections to database pods
- **Kubernetes**: Managed via StatefulSets with PVCs

## Backup and Recovery

MongoDB includes native backup tools:

```bash
# Backup using mongodump
mongodump --uri="mongodb://admin:adminpassword@localhost:27017" --out=/backup

# Restore using mongorestore
mongorestore --uri="mongodb://admin:adminpassword@localhost:27017" /backup
```

For replica sets, always backup from a secondary to avoid impacting the primary:

```bash
mongodump --uri="mongodb://admin:adminpassword@mongo-secondary1:27017/?authSource=admin&readPreference=secondary" --out=/backup
```

## Connecting to MongoDB

**Standalone Connection String:**
```
mongodb://admin:adminpassword@localhost:27017/?authSource=admin
```

**Replica Set Connection String:**
```
mongodb://admin:adminpassword@mongo-primary:27017,mongo-secondary1:27017,mongo-secondary2:27017/?authSource=admin&replicaSet=rs0
```

**Using mongosh:**
```bash
mongosh "mongodb://admin:adminpassword@localhost:27017/?authSource=admin"
```

**Using Node.js:**
```javascript
const { MongoClient } = require('mongodb');
const client = new MongoClient('mongodb://admin:adminpassword@localhost:27017/?authSource=admin');
await client.connect();
```

## Features

- **Document-oriented storage**: Flexible schema design
- **High performance**: Fast read/write operations
- **Scalability**: Horizontal scaling via sharding
- **Replication**: Built-in replication for high availability
- **Aggregation framework**: Powerful data processing
- **Automatic failover**: Replica sets elect new primary automatically

## License

ISC License - Copyright (c) Appwrite team
