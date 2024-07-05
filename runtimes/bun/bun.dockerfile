ENV OPEN_RUNTIMES_ENTRYPOINT=index.ts
COPY package* /usr/local/server/
RUN bun install
