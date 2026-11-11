# syntax=docker/dockerfile:1@sha256:4edf897a3ffa55b89f906fc8cc78afdb3f1834cc9c7083565e611a8a7d5fe99e
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

# static-web-server >= 2.40 images run as the unprivileged `sws` user. Switch
# back to root so the apk steps work and the runtime behaves as before.
USER root

INCLUDE ./docker/base-before

RUN apk add --no-cache bash apache2-utils

# Directory for configurable fallback files (like 404)
RUN mkdir -p /mnt/resources

INCLUDE ./docker/base-after
