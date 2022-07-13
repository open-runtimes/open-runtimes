FROM jarredsumner/bun:edge

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=index.js

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

RUN apk add --no-cache rsync

COPY package* /usr/local/src/

RUN bun install

COPY . .

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]