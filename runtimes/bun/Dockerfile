# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=index.ts
# Bun 1.0 arm64 includes glibc, whose /usr/lib/libc.so conflicts with musl-dev from g++.
RUN apk update \
  && APK_ADD_FLAGS="" \
  && if apk info -e glibc > /dev/null 2>&1; then APK_ADD_FLAGS="--force-overwrite"; fi \
  && apk add ${APK_ADD_FLAGS} bash npm nss font-noto ca-certificates python3 make g++ gcc git
RUN npm install pnpm@9.15.9 yarn modclean@2.1.2 -g
RUN bun install && npm install --no-save --prefix /usr/local/server @vercel/nft@^0.29.2

ENV OPEN_RUNTIMES_SERVER_COMMAND="bun src/server.ts"

INCLUDE ./docker/base-after
