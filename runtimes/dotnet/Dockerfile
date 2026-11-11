# syntax=docker/dockerfile:1@sha256:4edf897a3ffa55b89f906fc8cc78afdb3f1834cc9c7083565e611a8a7d5fe99e
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk add --no-cache bash

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.cs

ENV OPEN_RUNTIMES_SERVER_COMMAND="dotnet src/function/DotNetRuntime.dll"

INCLUDE ./docker/base-after
