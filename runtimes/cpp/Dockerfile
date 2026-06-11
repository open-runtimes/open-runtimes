# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=index.cc
ARG DEBIAN_FRONTEND=noninteractive
ENV DROGON_VERSION=v1.9.7

RUN apk add --no-cache \
    git \
    gcc \
    g++ \
    cmake \
    make \
    openssl \
    doxygen \
    jsoncpp-dev \
    openssl-dev \
    zlib-dev \
    curl-dev \
    util-linux-dev \
    bash

RUN git clone --depth 1 --branch $DROGON_VERSION https://github.com/drogonframework/drogon \
    && cd drogon \
    && git submodule update --init \
    && mkdir build \
    && cd build \
    && cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make -j"$(nproc)" \
    && make install \
    && cd / \
    && rm -rf /drogon

EXPOSE 3000

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/cpp_runtime"

INCLUDE ./docker/base-after
