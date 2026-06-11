# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

# Extra system packages installed before everything else (java >= 22: curl)
ARG SYSTEM_PACKAGES=""
RUN if [ -n "$SYSTEM_PACKAGES" ]; then apt-get update && apt-get install -y $SYSTEM_PACKAGES; fi

INCLUDE ./docker/base-before

RUN apt-get update && apt-get install -y zip bash

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.java
ENV OPEN_RUNTIMES_SERVER_COMMAND="java -jar src/function/java-runtime-1.0.0.jar"

INCLUDE ./docker/base-after
