# syntax = devthefuture/dockerfile-x:1.4.2
FROM node:14.5-alpine

INCLUDE ./base-before
INCLUDE ./node

# Override PNPM version: https://pnpm.io/installation#compatibility
RUN npm install pnpm@6 -g

INCLUDE ./base-after
