# MySQL/MariaDB Runtime

OpenRuntimes-based MySQL and MariaDB database runtimes for Appwrite dedicated databases.

## Overview

This runtime provides MySQL and MariaDB database engines wrapped in an OpenRuntimes management layer. It enables:

- **Cold-start support** for shared tier databases
- **Always-on deployment** for dedicated tier databases
- **Health check endpoints** for container orchestration
- **WAL-G integration** for continuous backups
- **Telemetry and monitoring** via OpenRuntimes management API

## Architecture

The runtime consists of two main components:

1. **MySQL/MariaDB Engine**: Native database server running on port 3306
2. **OpenRuntimes Management Layer**: Node.js server on port 3000 providing:
   - Health checks (`/__opr/health`)
   - Status monitoring (`/__opr/status`)
   - Telemetry (`/__opr/timings`)

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

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `MYSQL_ROOT_PASSWORD` | Root password | **YES** | (none - must provide) |
| `MYSQL_DATABASE` | Default database name | **YES** | (none - must provide) |
| `MYSQL_MAX_CONNECTIONS` | Maximum concurrent connections | No | `151` |

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
