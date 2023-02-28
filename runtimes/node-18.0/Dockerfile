FROM node:18.0-alpine

LABEL maintainer="team@appwrite.io"

ENV OPEN_RUNTIMES_ENTRYPOINT=index.js

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

RUN apk add --no-cache rsync

COPY package* /usr/local/src/

RUN npm install pm2 -g

RUN npm ci && npm cache clean --force

COPY . .

RUN chmod +x /usr/local/src/helpers/before_start.sh
RUN chmod +x /usr/local/src/helpers/start.sh

RUN chmod +x /usr/local/src/helpers/before_build.sh
RUN chmod +x /usr/local/src/helpers/build.sh
RUN chmod +x /usr/local/src/helpers/after_build.sh

EXPOSE 3000