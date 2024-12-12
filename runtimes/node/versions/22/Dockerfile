# syntax = devthefuture/dockerfile-x:1.4.2
FROM node:22.9.0-alpine3.20

# Temporary; Move to SSR runtime once created
RUN apk update && apk add --no-cache traefik

INCLUDE ./base-before
INCLUDE ./node
INCLUDE ./base-after
