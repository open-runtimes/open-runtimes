FROM mongo:8

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
LABEL org.opencontainers.image.source="https://github.com/open-runtimes/open-runtimes"

# OpenRuntimes environment variables
# IMPORTANT: All configuration must be provided at runtime via Kubernetes ConfigMap/Secret
# The runtime has no defaults - makes it stateless and secure by design
ENV OPEN_RUNTIMES_ENV=production

# MongoDB environment variables
# Required: MONGO_INITDB_ROOT_USERNAME, MONGO_INITDB_ROOT_PASSWORD, MONGO_INITDB_DATABASE

# Install Node.js for OpenRuntimes management layer
RUN apt-get update && \
    apt-get install -y nodejs npm curl && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /mnt/logs
RUN mkdir -p /mnt/telemetry
RUN mkdir -p /usr/local/server

WORKDIR /usr/local/server
COPY package*.json ./
RUN npm ci --omit=dev && npm cache clean --force

COPY src/ ./src/
COPY helpers/ ./helpers/
RUN chmod +x /usr/local/server/helpers/*.sh

EXPOSE 27017 3000

CMD ["/usr/local/server/helpers/server.sh"]
