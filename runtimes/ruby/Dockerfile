# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

ENV OPEN_RUNTIMES_ENTRYPOINT=index.rb

INCLUDE ./docker/base-before

RUN apk add --update alpine-sdk bash
RUN gem install bundler
ENV OPEN_RUNTIMES_SERVER_COMMAND="bundle exec puma -b tcp://0.0.0.0:3000 -e production"

INCLUDE ./docker/base-after
