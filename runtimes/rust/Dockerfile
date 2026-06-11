# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=main.rs

RUN apk update && apk add --no-cache \
    bash \
    musl-dev \
    openssl-dev \
    openssl-libs-static \
    pkgconfig \
    curl

RUN rustup component add rustfmt

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/rust_runtime"

INCLUDE ./docker/base-after
