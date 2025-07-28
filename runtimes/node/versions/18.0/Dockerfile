# syntax = devthefuture/dockerfile-x:1.4.2
FROM node:18.0.0-alpine3.15

INCLUDE ./base-before
INCLUDE ./node

# Override PNPM version: https://pnpm.io/installation#compatibility
RUN npm install pnpm@8 -g

INCLUDE ./base-after
