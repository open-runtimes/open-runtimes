# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk add --no-cache bash

ENV OPEN_RUNTIMES_ENTRYPOINT=main.go

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/server"

INCLUDE ./docker/base-after
