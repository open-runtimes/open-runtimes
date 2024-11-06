# syntax = devthefuture/dockerfile-x:1.4.2
FROM node:16.20.2-alpine3.18

INCLUDE ./base-before
INCLUDE ./node

# Override PNPM version: https://pnpm.io/installation#compatibility
RUN npm install pnpm@8 -g

INCLUDE ./base-after
