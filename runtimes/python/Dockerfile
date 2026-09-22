# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

# Extra system packages installed before everything else (used by ml variants:
# "gcc pkg-config libhdf5-dev" on a bookworm base)
ARG SYSTEM_PACKAGES=""
RUN if [ -n "$SYSTEM_PACKAGES" ]; then \
        if [ -f /etc/alpine-release ]; then apk add --no-cache $SYSTEM_PACKAGES; \
        else apt-get update && apt-get install -y $SYSTEM_PACKAGES; fi; \
    fi

INCLUDE ./docker/base-before

RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk add --no-cache bash alpine-sdk libffi-dev
    else
        apt-get update && apt-get install -y bash
    fi
EOR

# Install Poetry package manager
RUN pip3 install --no-cache-dir poetry

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

# Create a virtual environment for the server dependencies
RUN python3 -m venv server-env
RUN server-env/bin/pip install --no-cache-dir -r requirements.txt

INCLUDE ./docker/base-after
