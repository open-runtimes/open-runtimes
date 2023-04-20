FROM oven/bun:0.5.9

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=index.ts

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY package.json /usr/local/src/
COPY bun.lockb /usr/local/src/

COPY . .

RUN bun install

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]
