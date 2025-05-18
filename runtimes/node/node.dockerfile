COPY package* /usr/local/server/

RUN apk update && apk add bash

RUN npm install pnpm@9 -g

RUN npm ci && npm cache clean --force
