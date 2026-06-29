# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk update && apk add bash nss font-noto ca-certificates gcompat

# https://pnpm.io/installation#compatibility (node 16 needs pnpm 8)
ARG PNPM_VERSION=9.15.9
RUN npm install pnpm@${PNPM_VERSION} modclean@2.1.2 -g

RUN npm ci && npm cache clean --force

# Additional tooling needed for Node runtimes used for SSR (Sites)
# alpine-sdk - basic tooling like git, gcc, g++, make...
# python3-dev - Install Python (useful to some NPM libs)
# py3-pip - Python package manager
# gcompat - Pre-requirement for Bun
ARG INSTALL_SSR_TOOLING="false"
ARG BUN_VERSION=1.3.2
RUN if [ "$INSTALL_SSR_TOOLING" = "true" ]; then \
        apk add alpine-sdk python3-dev py3-pip gcompat && \
        npm install bun@${BUN_VERSION} -g; \
    fi

ENV OPEN_RUNTIMES_SERVER_COMMAND="node --max_old_space_size=8192 src/server.js"

# Next.js packing: keep node's pre-consolidation behavior (ship node_modules
# untouched) instead of the shared packer's modclean fallback used by bun/deno.
ENV OPEN_RUNTIMES_NEXTJS_CLEANUP=none

INCLUDE ./docker/base-after
