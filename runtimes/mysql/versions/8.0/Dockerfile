FROM mysql:8.0

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"
ENV OPEN_RUNTIMES_ENV=production

# MySQL environment variables
# Required: MYSQL_ROOT_PASSWORD, MYSQL_DATABASE
# Optional: MYSQL_USER, MYSQL_PASSWORD (for additional non-root user)

RUN microdnf install -y nodejs npm curl tar gzip unzip && \
    microdnf clean all

# Install WAL-G for MySQL backup management
ENV WALG_VERSION=v3.0.4
RUN microdnf install -y wget && \
    wget -O /usr/local/bin/wal-g https://github.com/wal-g/wal-g/releases/download/${WALG_VERSION}/wal-g-mysql-ubuntu-20.04-amd64 && \
    chmod +x /usr/local/bin/wal-g && \
    microdnf remove -y wget && \
    microdnf clean all

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

VOLUME ["/var/lib/mysql"]
VOLUME ["/mnt/backups"]

EXPOSE 3306 3000

CMD ["/usr/local/server/helpers/server.sh"]
