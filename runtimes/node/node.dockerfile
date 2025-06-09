COPY package* /usr/local/server/

RUN apk update && apk add bash alpine-sdk python3-dev

RUN npm install pnpm@9.15.9 bun@1.2.15 -g

RUN npm ci && npm cache clean --force
