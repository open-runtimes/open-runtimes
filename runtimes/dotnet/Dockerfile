# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.cs

ENV OPEN_RUNTIMES_SERVER_COMMAND="dotnet src/function/DotNetRuntime.dll"

INCLUDE ./docker/base-after
