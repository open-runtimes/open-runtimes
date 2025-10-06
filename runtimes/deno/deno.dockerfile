RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache