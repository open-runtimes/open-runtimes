# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=

RUN apt-get update && apt-get install -y zip bash
ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/Runtime serve --env production --hostname 0.0.0.0 --port 3000"

INCLUDE ./docker/base-after
