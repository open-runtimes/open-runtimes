FROM postgres:16-alpine3.22

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"

# OpenRuntimes environment variables
# IMPORTANT: All configuration must be provided at runtime via Kubernetes ConfigMap/Secret
# The runtime has no defaults - makes it stateless and secure by design
ENV OPEN_RUNTIMES_ENV=production

# PostgreSQL environment variables
# Required: POSTGRES_USER, POSTGRES_PASSWORD, POSTGRES_DB
# Optional: PGDATA (defaults to /var/lib/postgresql/data by postgres image)

# Install Node.js for OpenRuntimes management layer
RUN apk add --no-cache nodejs npm bash curl util-linux unzip

# Install glibc compatibility for wal-g (which is built for glibc-based systems)
ENV WALG_VERSION=v3.0.4
RUN apk add --no-cache wget libc6-compat && \
    wget -O /usr/local/bin/wal-g https://github.com/wal-g/wal-g/releases/download/${WALG_VERSION}/wal-g-pg-ubuntu-20.04-amd64 && \
    chmod +x /usr/local/bin/wal-g && \
    apk del wget

RUN mkdir -p /mnt/code
RUN mkdir -p /mnt/logs
RUN mkdir -p /mnt/telemetry
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server
COPY package*.json ./
RUN npm ci --omit=dev && npm cache clean --force

COPY src/ ./src/
COPY helpers/ ./helpers/
RUN chmod +x /usr/local/server/helpers/*.sh

EXPOSE 5432 3000

CMD ["/usr/local/server/helpers/server.sh"]
