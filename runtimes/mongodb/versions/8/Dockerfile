FROM mongo:8

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"
ENV OPEN_RUNTIMES_ENV=production

# MongoDB environment variables
# Required: MONGO_INITDB_ROOT_USERNAME, MONGO_INITDB_ROOT_PASSWORD, MONGO_INITDB_DATABASE

RUN apt-get update && \
    apt-get install -y nodejs npm curl unzip && \
    rm -rf /var/lib/apt/lists/*

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

VOLUME ["/data/db"]
VOLUME ["/data/configdb"]
VOLUME ["/mnt/backups"]

EXPOSE 27017 3000

CMD ["/usr/local/server/helpers/server.sh"]
