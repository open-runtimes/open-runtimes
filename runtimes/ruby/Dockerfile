# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

ENV OPEN_RUNTIMES_ENTRYPOINT=index.rb

INCLUDE ./docker/base-before

RUN apk add --no-cache alpine-sdk bash
RUN gem install bundler
ENV OPEN_RUNTIMES_SERVER_COMMAND="bundle exec puma -b tcp://0.0.0.0:3000 -e production"

INCLUDE ./docker/base-after
