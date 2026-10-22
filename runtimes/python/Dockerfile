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

# Older Pythons bundle a pip whose vendored certifi still trusts the
# compromised eMudhra CAs (GHSA-6q48-6c7w-9gw2). Point every pip, including the
# ensurepip copy in venvs created at build time, at the OS trust store, and
# upgrade the image's own pips. pip >= 26.1 needs Python 3.10+, so 3.8/3.9 pin
# their last compatible release via PIP_VERSION.
ARG PIP_VERSION=26.2.2
RUN test -f /etc/ssl/certs/ca-certificates.crt && \
    printf '[global]\ncert = /etc/ssl/certs/ca-certificates.crt\n' > /etc/pip.conf && \
    pip3 install --no-cache-dir --upgrade "pip==${PIP_VERSION}"

# Install Poetry package manager
RUN pip3 install --no-cache-dir poetry

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

# Create a virtual environment for the server dependencies
RUN python3 -m venv server-env
RUN server-env/bin/pip install --no-cache-dir --upgrade "pip==${PIP_VERSION}" && \
    server-env/bin/pip install --no-cache-dir -r requirements.txt

INCLUDE ./docker/base-after
