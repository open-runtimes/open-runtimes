COPY package* /usr/local/server/

RUN apk add --no-cache \
    build-base \
    python3 \
    make \
    gcc \
    g++

RUN npm install pnpm@9 -g

RUN npm ci && npm cache clean --force

RUN chmod +x /usr/local/server/helpers/proxy.sh
