# MySQL/MariaDB Runtime

OpenRuntimes-based MySQL and MariaDB database runtimes for Appwrite dedicated databases.

## Overview

This runtime provides MySQL and MariaDB database engines wrapped in an OpenRuntimes management layer. It enables:

- **Cold-start support** for shared tier databases
- **Always-on deployment** for dedicated tier databases
- **Health check endpoints** for container orchestration
- **WAL-G integration** for continuous backups
- **Telemetry and monitoring** via OpenRuntimes management API
- **Primary/Replica replication** for high availability

## Architecture

The runtime consists of two main components:

1. **MySQL/MariaDB Engine**: Native database server running on port 3306
2. **OpenRuntimes Management Layer**: Node.js server on port 3000 providing:
   - Health checks (`/__opr/health`)
   - Status monitoring (`/__opr/status`)
   - Telemetry (`/__opr/timings`)
   - Replication management (`/__opr/replication`)

## Available Versions

- MySQL 8.4
- MariaDB 11

## Usage

### Building the Runtime

**MySQL:**
```bash
docker build -t appwrite/mysql:8.4-runtime runtimes/mysql/versions/8.4/
```

**MariaDB:**
```bash
docker build -t appwrite/mariadb:11-runtime runtimes/mysql/versions/mariadb-11/
```

### Running the Container

**⚠️ IMPORTANT: Credentials must be provided at runtime and should NEVER be hardcoded.**

**MySQL:**
```bash
# Generate secure password
MYSQL_ROOT_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  -p 3306:3306 \
  -p 3000:3000 \
  -e MYSQL_ROOT_PASSWORD="$MYSQL_ROOT_PASSWORD" \
  -e MYSQL_DATABASE=mydb \
  -v mysqldata:/var/lib/mysql \
  appwrite/mysql:8.4-runtime
```

**MariaDB:**
```bash
# Generate secure password
MYSQL_ROOT_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  -p 3306:3306 \
  -p 3000:3000 \
  -e MYSQL_ROOT_PASSWORD="$MYSQL_ROOT_PASSWORD" \
  -e MYSQL_DATABASE=mydb \
  -v mariadbdata:/var/lib/mysql \
  appwrite/mariadb:11-runtime
```

### Environment Variables

#### Basic Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MYSQL_ROOT_PASSWORD` | Root password | **YES** | (none - must provide) |
| `MYSQL_DATABASE` | Default database name | **YES** | (none - must provide) |
| `MYSQL_MAX_CONNECTIONS` | Maximum concurrent connections | No | `151` |

#### Replication Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MYSQL_REPLICATION_MODE` | Replication mode: `standalone`, `primary`, or `replica` | No | `standalone` |
| `MYSQL_SERVER_ID` | Unique server ID (must be different for each server) | No | `1` |
| `MYSQL_REPLICATION_USER` | Replication user name | No | `replicator` |
| `MYSQL_REPLICATION_PASSWORD` | Replication user password | For replication | (none) |
| `MYSQL_PRIMARY_HOST` | Primary server hostname (for replicas) | For replicas | (none) |
| `MYSQL_PRIMARY_PORT` | Primary server port | No | `3306` |
| `MYSQL_GTID_MODE` | Enable GTID-based replication | No | `ON` |
| `MYSQL_ENFORCE_GTID_CONSISTENCY` | Enforce GTID consistency | No | `ON` |
| `MYSQL_BINLOG_FORMAT` | Binary log format: `ROW`, `STATEMENT`, or `MIXED` | No | `ROW` |
| `MYSQL_BINLOG_ROW_IMAGE` | Row image for binary log: `FULL`, `MINIMAL`, or `NOBLOB` | No | `FULL` |
| `MYSQL_SYNC_BINLOG` | Binary log sync frequency | No | `1` |
| `MYSQL_RELAY_LOG_RECOVERY` | Enable relay log recovery | No | `ON` |
| `MYSQL_READ_ONLY` | Set replica to read-only | No | `ON` (for replicas) |
| `MYSQL_SUPER_READ_ONLY` | Set replica to super read-only | No | `ON` (for replicas) |
| `MYSQL_REPLICATE_DO_DB` | Comma-separated list of databases to replicate | No | (all) |
| `MYSQL_REPLICATE_IGNORE_DB` | Comma-separated list of databases to ignore | No | (none) |
| `MYSQL_REPLICATION_CONNECT_RETRY` | Seconds between reconnection attempts | No | `60` |
| `MYSQL_SEMI_SYNC_ENABLED` | Enable semi-synchronous replication | No | `false` |
| `MYSQL_SEMI_SYNC_TIMEOUT` | Semi-sync timeout in milliseconds | No | `10000` |

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
# Returns JSON with database status, connections, uptime, and replication info
```

## Replication

### Setting Up Primary/Replica Replication

#### Primary Server

```bash
MYSQL_ROOT_PASSWORD=$(openssl rand -base64 32)
MYSQL_REPLICATION_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  --name mysql-primary \
  -p 3306:3306 \
  -p 3000:3000 \
  -e MYSQL_ROOT_PASSWORD="$MYSQL_ROOT_PASSWORD" \
  -e MYSQL_DATABASE=mydb \
  -e MYSQL_REPLICATION_MODE=primary \
  -e MYSQL_SERVER_ID=1 \
  -e MYSQL_REPLICATION_USER=replicator \
  -e MYSQL_REPLICATION_PASSWORD="$MYSQL_REPLICATION_PASSWORD" \
  -v mysql-primary-data:/var/lib/mysql \
  appwrite/mysql:8.4-runtime
```

#### Replica Server

```bash
docker run -d \
  --name mysql-replica \
  -p 3307:3306 \
  -p 3001:3000 \
  -e MYSQL_ROOT_PASSWORD="$MYSQL_ROOT_PASSWORD" \
  -e MYSQL_DATABASE=mydb \
  -e MYSQL_REPLICATION_MODE=replica \
  -e MYSQL_SERVER_ID=2 \
  -e MYSQL_PRIMARY_HOST=mysql-primary \
  -e MYSQL_PRIMARY_PORT=3306 \
  -e MYSQL_REPLICATION_USER=replicator \
  -e MYSQL_REPLICATION_PASSWORD="$MYSQL_REPLICATION_PASSWORD" \
  -v mysql-replica-data:/var/lib/mysql \
  appwrite/mysql:8.4-runtime
```

### Replication Endpoints

#### Get Replication Status

```bash
curl http://localhost:3000/__opr/replication
```

Returns detailed replication status including:
- Primary: binlog position, GTID set, connected replicas
- Replica: replication lag, IO/SQL thread status, errors

#### Start Replication (Replica only)

```bash
curl -X POST http://localhost:3000/__opr/replication/start
```

#### Stop Replication (Replica only)

```bash
curl -X POST http://localhost:3000/__opr/replication/stop
```

#### Reset Replication (Replica only)

```bash
curl -X POST http://localhost:3000/__opr/replication/reset
```

### Semi-Synchronous Replication

For stronger durability guarantees, enable semi-synchronous replication:

```bash
# Primary
-e MYSQL_SEMI_SYNC_ENABLED=true \
-e MYSQL_SEMI_SYNC_TIMEOUT=10000

# Replica
-e MYSQL_SEMI_SYNC_ENABLED=true
```

With semi-sync, the primary waits for at least one replica to acknowledge receipt of each transaction before committing.

### GTID vs Position-Based Replication

By default, GTID (Global Transaction Identifier) replication is enabled, which provides:
- Easier failover and replica promotion
- Automatic position tracking
- Simpler topology changes

To use traditional position-based replication, set:
```bash
-e MYSQL_GTID_MODE=OFF
```

## Integration with Appwrite

This runtime is designed to be used with Appwrite's dedicated database infrastructure:

- **Shared Tier**: Automatic cold-start and pause lifecycle
- **Dedicated Tier**: Always-on deployment with guaranteed resources
- **TCP Proxy**: Routes connections to database pods
- **Kubernetes**: Managed via StatefulSets with PVCs

## Backup and Recovery

WAL-G is included for continuous backup support:

```bash
# Configure WAL-G environment
export WALG_S3_PREFIX=s3://my-bucket/mysql-backups
export AWS_ACCESS_KEY_ID=...
export AWS_SECRET_ACCESS_KEY=...

# Backup
wal-g backup-push

# Restore
wal-g backup-fetch LATEST
```

## Differences Between MySQL and MariaDB

Both runtimes use the same architecture and management layer. Key differences:

- **MySQL 8.4**: Official Oracle MySQL with InnoDB storage engine
- **MariaDB 11**: Community-developed fork with additional features and Aria storage engine
- Both use port 3306 and are compatible with the MySQL protocol
- mysql2 Node.js driver supports both engines

## License

ISC License - Copyright (c) Appwrite team
