RUN apk update && apk add bash nss font-noto ca-certificates gcompat

ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache