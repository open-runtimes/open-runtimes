# syntax = devthefuture/dockerfile-x:1.4.2
FROM node:22.22.0-alpine3.23

INCLUDE ./base-before
INCLUDE ./node

# Additional tooling needed for Node runtimes used for SSR (Sites)
# alpine-sdk - basic tooling like git, gcc, g++, make...
# python3-dev - Install Python (useful to some NPM libs)
# py3-pip - Python package manager
# gcompat - Pre-requirement for Bun
RUN apk add alpine-sdk python3-dev py3-pip gcompat
RUN npm install bun@1.3.2 -g

INCLUDE ./base-after
