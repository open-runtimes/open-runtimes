# syntax = devthefuture/dockerfile-x:1.4.2
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk update && apk add bash apache2-utils

# Directory for configurable fallback files (like 404)
RUN mkdir -p /mnt/resources

INCLUDE ./docker/base-after
