# MongoDB Runtime

OpenRuntimes-based MongoDB database runtime for Appwrite dedicated databases.

## Overview

This runtime provides a MongoDB database engine wrapped in an OpenRuntimes management layer. It enables:

- **Cold-start support** for shared tier databases
- **Always-on deployment** for dedicated tier databases
- **Health check endpoints** for container orchestration
- **Telemetry and monitoring** via OpenRuntimes management API

## Architecture

The runtime consists of two main components:

1. **MongoDB Engine**: Native MongoDB server running on port 27017
2. **OpenRuntimes Management Layer**: Node.js server on port 3000 providing:
   - Health checks (`/__opr/health`)
   - Status monitoring (`/__opr/status`)
   - Telemetry (`/__opr/timings`)

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

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MONGO_INITDB_ROOT_USERNAME` | Root username | **YES** | (none - must provide) |
| `MONGO_INITDB_ROOT_PASSWORD` | Root password | **YES** | (none - must provide) |
| `MONGO_INITDB_DATABASE` | Authentication database | **YES** | (none - must provide) |

**Security Notes:**
- The container will **fail to start** if required environment variables are not provided
- All database configuration must be provided at runtime (stateless design)
- Passwords should be generated securely (32+ characters, cryptographically random)
- In production, credentials are injected via Kubernetes Secrets
- Never commit credentials to version control

### Health Check

```bash
curl http://localhost:3000/__opr/health
# Response: OK (200) if healthy, 503 if not ready
```

### Status Check

```bash
curl http://localhost:3000/__opr/status
# Returns JSON with database status, connections, uptime
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

## Connecting to MongoDB

**Connection String:**
```
mongodb://admin:adminpassword@localhost:27017/?authSource=admin
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

## License

ISC License - Copyright (c) Appwrite team
