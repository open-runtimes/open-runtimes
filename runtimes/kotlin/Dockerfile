# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.kt

ENV OPEN_RUNTIMES_SERVER_COMMAND="java -jar src/function/kotlin-runtime-1.0.0.jar"

INCLUDE ./docker/base-after
