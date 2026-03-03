# syntax = devthefuture/dockerfile-x:1.4.2

# ---- Build image (used for npm install / SSR builds) ----
FROM node:23.11.1-alpine3.22 AS build

INCLUDE ./base-before
INCLUDE ./node

RUN apk add --no-cache alpine-sdk python3-dev py3-pip font-noto \
    && npm install bun@1.3.2 -g \
    && npm cache clean --force && rm -rf /tmp/*

INCLUDE ./base-after

# ---- Runtime image (used for serving) ----
FROM node:23.11.1-alpine3.22 AS runtime

INCLUDE ./base-before
INCLUDE ./node-runtime
INCLUDE ./base-after
