FROM mysql:8.4

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"

# OpenRuntimes environment variables
# IMPORTANT: All configuration must be provided at runtime via Kubernetes ConfigMap/Secret
# The runtime has no defaults - makes it stateless and secure by design
ENV OPEN_RUNTIMES_ENV=production

# MySQL environment variables
# Required: MYSQL_ROOT_PASSWORD, MYSQL_DATABASE
# Optional: MYSQL_USER, MYSQL_PASSWORD (for additional non-root user)

# Install Node.js for OpenRuntimes management layer
# MySQL 8.4 image is based on Oracle Linux minimal, so use microdnf
RUN microdnf install -y nodejs npm curl tar gzip && \
    microdnf clean all

# Install WAL-G for MySQL backup management
ENV WALG_VERSION=v3.0.4
RUN microdnf install -y wget && \
    wget -O /usr/local/bin/wal-g https://github.com/wal-g/wal-g/releases/download/${WALG_VERSION}/wal-g-mysql-ubuntu-20.04-amd64 && \
    chmod +x /usr/local/bin/wal-g && \
    microdnf remove -y wget && \
    microdnf clean all

RUN mkdir -p /mnt/logs
RUN mkdir -p /mnt/telemetry
RUN mkdir -p /usr/local/server

WORKDIR /usr/local/server
COPY package*.json ./
RUN npm ci --omit=dev && npm cache clean --force

COPY src/ ./src/
COPY helpers/ ./helpers/
RUN chmod +x /usr/local/server/helpers/*.sh

EXPOSE 3306 3000

CMD ["/usr/local/server/helpers/server.sh"]
