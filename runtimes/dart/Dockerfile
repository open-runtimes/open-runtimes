# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apt-get update && apt-get install -y bash

RUN dart --disable-analytics

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/server"

INCLUDE ./docker/base-after
