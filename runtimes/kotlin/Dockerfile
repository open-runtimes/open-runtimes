# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.kt

ENV OPEN_RUNTIMES_SERVER_COMMAND="java -jar src/function/kotlin-runtime-1.0.0.jar"

INCLUDE ./docker/base-after
