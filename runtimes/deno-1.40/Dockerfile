FROM denoland/deno:alpine-1.41.2

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts

RUN mkdir -p /mnt/code
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

ENV DENO_DIR=/usr/builds/deno-cache

RUN deno install -qAf --unstable https://deno.land/x/denon@2.5.0/denon.ts

COPY . .

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

EXPOSE 3000
