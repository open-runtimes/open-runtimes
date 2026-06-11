# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apt-get update && apt-get install -y bash

RUN dart --disable-analytics

INCLUDE ./docker/base-after

# Enable web platform
RUN flutter config --enable-web
