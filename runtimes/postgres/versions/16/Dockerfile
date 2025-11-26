FROM postgres:16

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"
ENV OPEN_RUNTIMES_ENV=production

# PostgreSQL environment variables
# Required: POSTGRES_USER, POSTGRES_PASSWORD, POSTGRES_DB
# Optional: PGDATA (defaults to /var/lib/postgresql/data by postgres image)

RUN apk add --no-cache \
    bash \
    curl \
    nodejs \
    npm \
    unzip \
    util-linux

ENV WALG_VERSION=v3.0.4
RUN wget -O /usr/local/bin/wal-g https://github.com/wal-g/wal-g/releases/download/${WALG_VERSION}/wal-g-pg-ubuntu-20.04-amd64 && \
    chmod +x /usr/local/bin/wal-g && \
    apk del wget

RUN mkdir -p /mnt/backups \
 && mkdir -p /mnt/code \
 && mkdir -p /mnt/logs \
 && mkdir -p /mnt/telemetry \
 && mkdir -p /usr/local/build \
 && mkdir -p /usr/local/server \
 && mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server
COPY package*.json ./
RUN npm ci --omit=dev && npm cache clean --force

COPY src/ ./src/
COPY helpers/ ./helpers/
RUN chmod +x /usr/local/server/helpers/*.sh

VOLUME ["/var/lib/postgresql/data"]
VOLUME ["/mnt/backups"]

EXPOSE 5432 3000

CMD ["/usr/local/server/helpers/server.sh"]
