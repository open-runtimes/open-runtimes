COPY package* /usr/local/server/

RUN apk add --no-cache bash nss ca-certificates gcompat \
    && npm ci && npm cache clean --force && rm -rf /tmp/*
