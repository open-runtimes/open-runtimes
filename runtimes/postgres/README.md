# PostgreSQL Runtime

OpenRuntimes-based PostgreSQL database runtime for Appwrite dedicated databases.

## Overview

This runtime provides a PostgreSQL database engine wrapped in an OpenRuntimes management layer. It enables:

- **Cold-start support** for shared tier databases
- **Always-on deployment** for dedicated tier databases
- **Health check endpoints** for container orchestration
- **WAL-G integration** for continuous backups and PITR
- **Telemetry and monitoring** via OpenRuntimes management API
- **Streaming replication** for high availability

## Architecture

The runtime consists of two main components:

1. **PostgreSQL Engine**: Native PostgreSQL server running on port 5432
2. **OpenRuntimes Management Layer**: Node.js server on port 3000 providing:
   - Health checks (`/__opr/health`)
   - Status monitoring (`/__opr/status`)
   - Telemetry (`/__opr/timings`)
   - Replication management (`/__opr/replication`)

## Available Versions

- PostgreSQL 16 (Alpine-based)

## Usage

### Building the Runtime

```bash
docker build -t appwrite/postgres:16-runtime runtimes/postgres/versions/16/
```

### Running the Container

**⚠️ IMPORTANT: Credentials must be provided at runtime and should NEVER be hardcoded.**

```bash
# Generate secure password
POSTGRES_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  -p 5432:5432 \
  -p 3000:3000 \
  -e POSTGRES_USER=myuser \
  -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
  -e POSTGRES_DB=mydb \
  -v pgdata:/var/lib/postgresql/data \
  appwrite/postgres:16-runtime
```

### Environment Variables

#### Basic Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `POSTGRES_USER` | Database superuser name | **YES** | (none - must provide) |
| `POSTGRES_PASSWORD` | Database superuser password | **YES** | (none - must provide) |
| `POSTGRES_DB` | Default database name | **YES** | (none - must provide) |
| `POSTGRES_MAX_CONNECTIONS` | Maximum concurrent connections | No | `100` |
| `PGDATA` | Data directory path | No | `/var/lib/postgresql/data` |

#### Replication Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `POSTGRES_REPLICATION_MODE` | Replication mode: `standalone`, `primary`, or `replica` | No | `standalone` |
| `POSTGRES_REPLICATION_USER` | Replication user name | No | `replicator` |
| `POSTGRES_REPLICATION_PASSWORD` | Replication user password | For replication | (none) |
| `POSTGRES_PRIMARY_HOST` | Primary server hostname (for replicas) | For replicas | (none) |
| `POSTGRES_PRIMARY_PORT` | Primary server port | No | `5432` |
| `POSTGRES_REPLICATION_SLOT_NAME` | Replication slot name | No | `replica_slot` |
| `POSTGRES_WAL_LEVEL` | WAL level: `replica`, `logical` | No | `replica` |
| `POSTGRES_MAX_WAL_SENDERS` | Maximum WAL sender processes | No | `10` |
| `POSTGRES_MAX_REPLICATION_SLOTS` | Maximum replication slots | No | `10` |
| `POSTGRES_WAL_KEEP_SIZE` | Minimum WAL to keep for replicas | No | `1GB` |
| `POSTGRES_HOT_STANDBY` | Enable read queries on replica | No | `on` |
| `POSTGRES_HOT_STANDBY_FEEDBACK` | Send feedback to primary | No | `on` |
| `POSTGRES_SYNCHRONOUS_COMMIT` | Synchronous commit level | No | `on` |
| `POSTGRES_SYNCHRONOUS_STANDBY_NAMES` | Synchronous standby names | No | (none) |
| `POSTGRES_ARCHIVE_MODE` | Enable WAL archiving | No | `off` |
| `POSTGRES_ARCHIVE_COMMAND` | Command to archive WAL files | No | (none) |
| `POSTGRES_RESTORE_COMMAND` | Command to restore WAL files | No | (none) |
| `POSTGRES_RECOVERY_TARGET_TIMELINE` | Recovery timeline target | No | `latest` |
| `POSTGRES_PRIMARY_CONNINFO_EXTRA` | Extra connection parameters | No | (none) |
| `POSTGRES_REPLICATION_ALLOWED_NETWORKS` | CIDR for replication connections | No | `0.0.0.0/0` |

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

### Setting Up Streaming Replication

#### Primary Server

```bash
POSTGRES_PASSWORD=$(openssl rand -base64 32)
REPLICATION_PASSWORD=$(openssl rand -base64 32)

docker run -d \
  --name postgres-primary \
  -p 5432:5432 \
  -p 3000:3000 \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
  -e POSTGRES_DB=mydb \
  -e POSTGRES_REPLICATION_MODE=primary \
  -e POSTGRES_REPLICATION_USER=replicator \
  -e POSTGRES_REPLICATION_PASSWORD="$REPLICATION_PASSWORD" \
  -v postgres-primary-data:/var/lib/postgresql/data \
  appwrite/postgres:16-runtime
```

#### Replica Server

```bash
docker run -d \
  --name postgres-replica \
  -p 5433:5432 \
  -p 3001:3000 \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
  -e POSTGRES_DB=mydb \
  -e POSTGRES_REPLICATION_MODE=replica \
  -e POSTGRES_PRIMARY_HOST=postgres-primary \
  -e POSTGRES_PRIMARY_PORT=5432 \
  -e POSTGRES_REPLICATION_USER=replicator \
  -e POSTGRES_REPLICATION_PASSWORD="$REPLICATION_PASSWORD" \
  -v postgres-replica-data:/var/lib/postgresql/data \
  appwrite/postgres:16-runtime
```

The replica will automatically:
1. Perform a base backup from the primary (if data directory is empty)
2. Create the `standby.signal` file
3. Configure streaming replication
4. Start following the primary

### Replication Endpoints

#### Get Replication Status

```bash
curl http://localhost:3000/__opr/replication
```

Returns detailed replication status including:
- Primary: current WAL LSN, connected standbys, replication slots
- Replica: receive/replay LSN, replication lag, WAL receiver status

#### Promote Replica to Primary

```bash
curl -X POST http://localhost:3000/__opr/replication/promote
```

Promotes a replica to become a standalone primary server.

#### Pause WAL Replay (Replica only)

```bash
curl -X POST http://localhost:3000/__opr/replication/pause
```

Pauses WAL replay, freezing the replica at its current state (useful for consistent backups).

#### Resume WAL Replay (Replica only)

```bash
curl -X POST http://localhost:3000/__opr/replication/resume
```

Resumes WAL replay after a pause.

#### Create Replication Slot (Primary only)

```bash
curl -X POST http://localhost:3000/__opr/replication/slot/my_slot_name
```

Creates a new physical replication slot.

#### Drop Replication Slot (Primary only)

```bash
curl -X DELETE http://localhost:3000/__opr/replication/slot/my_slot_name
```

Drops an existing replication slot.

### Synchronous Replication

For stronger durability guarantees, enable synchronous replication:

```bash
# Primary
-e POSTGRES_SYNCHRONOUS_COMMIT=on \
-e POSTGRES_SYNCHRONOUS_STANDBY_NAMES='replica1'

# Replica (set application_name)
-e POSTGRES_PRIMARY_CONNINFO_EXTRA='application_name=replica1'
```

With synchronous replication, the primary waits for the standby to confirm WAL receipt before committing.

### Hot Standby

By default, replicas run in hot standby mode (`POSTGRES_HOT_STANDBY=on`), allowing read-only queries. This enables:
- Load balancing read queries across replicas
- Reporting workloads on replicas
- Zero-downtime backups from replicas

## Integration with Appwrite

This runtime is designed to be used with Appwrite's dedicated database infrastructure:

- **Shared Tier**: Automatic cold-start and pause lifecycle
- **Dedicated Tier**: Always-on deployment with guaranteed resources
- **TCP Proxy**: Routes connections to database pods
- **Kubernetes**: Managed via StatefulSets with PVCs

### Kubernetes Deployment

In production, credentials are managed via Kubernetes Secrets:

```yaml
# Generated by Appwrite Compute API during provisioning
apiVersion: v1
kind: Secret
metadata:
  name: db-credentials
  namespace: db-abc123
type: Opaque
data:
  username: cG9zdGdyZXM=        # base64: postgres
  password: <base64-encoded-random-password>

---
apiVersion: v1
kind: Pod
metadata:
  name: db-abc123
  namespace: db-abc123
spec:
  containers:
  - name: postgres
    image: appwrite/postgres:16-runtime
    env:
    - name: POSTGRES_USER
      valueFrom:
        secretKeyRef:
          name: db-credentials
          key: username
    - name: POSTGRES_PASSWORD
      valueFrom:
        secretKeyRef:
          name: db-credentials
          key: password
    - name: POSTGRES_DB
      value: "postgres"
```

**Password Generation** happens in the Appwrite API:
```php
// Example from Appwrite Compute API
$password = bin2hex(random_bytes(32)); // 64-character hex string
```

## Backup and Recovery

WAL-G is included for continuous WAL archiving and point-in-time recovery (PITR):

```bash
# Configure WAL-G environment
export WALG_S3_PREFIX=s3://my-bucket/postgres-backups
export AWS_ACCESS_KEY_ID=...
export AWS_SECRET_ACCESS_KEY=...

# Backup
wal-g backup-push $PGDATA

# Restore
wal-g backup-fetch $PGDATA LATEST
```

## License

ISC License - Copyright (c) Appwrite team
