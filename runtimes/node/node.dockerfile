COPY package* /usr/local/server/

RUN apk update && apk add bash

RUN npm install pnpm@9.15.9 modclean@2.1.2 -g

RUN npm ci && npm cache clean --force
