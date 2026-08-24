# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apt-get update && apt-get install -y bash jq

RUN dart --disable-analytics

INCLUDE ./docker/base-after

# Enable web platform
RUN flutter config --enable-web

# Preinstall the static file server; 4.1.0 is the newest release compatible
# with every Dart SDK in the matrix
RUN dart pub global activate dhttpd 4.1.0
