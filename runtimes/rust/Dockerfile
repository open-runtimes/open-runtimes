# syntax=docker/dockerfile:1@sha256:4edf897a3ffa55b89f906fc8cc78afdb3f1834cc9c7083565e611a8a7d5fe99e
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=main.rs

RUN apk add --no-cache \
    bash \
    musl-dev \
    openssl-dev \
    openssl-libs-static \
    pkgconfig \
    curl

RUN rustup component add rustfmt

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/rust_runtime"

INCLUDE ./docker/base-after
