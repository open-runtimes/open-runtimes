# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE} AS flutter-base

# Flutter's Linux SDK bundles glibc binaries, so use a slim Debian base rather
# than Alpine. Keep all OS dependencies in this version-independent layer.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        git \
        jq \
        libglu1-mesa \
        unzip \
        xz-utils \
        zip \
    && rm -rf /var/lib/apt/lists/*

ENV FLUTTER_HOME=/opt/flutter
ENV FLUTTER_ROOT=${FLUTTER_HOME}
ENV PUB_CACHE=/root/.pub-cache
ENV PATH=${FLUTTER_HOME}/bin:${FLUTTER_HOME}/bin/cache/dart-sdk/bin:${PUB_CACHE}/bin:${PATH}
ENV BOT=true

# Build the version-specific SDK separately so changes to runtime helpers do
# not invalidate the expensive Flutter download and precache step.
FROM flutter-base AS flutter-sdk

ARG FLUTTER_VERSION

RUN git clone --depth 1 --branch "${FLUTTER_VERSION}" \
        https://github.com/flutter/flutter.git "${FLUTTER_HOME}" \
    && flutter precache --web

FROM flutter-base

INCLUDE ./docker/base-before

INCLUDE ./docker/base-after

# This is the only large version-specific layer in the final image; all OS and
# Open Runtimes layers above it are shared by every Flutter version.
COPY --from=flutter-sdk ${FLUTTER_HOME} ${FLUTTER_HOME}

# Enable web platform
RUN flutter config --enable-web \
    && dart --disable-analytics

# Preinstall the static file server; 4.1.0 is the newest release compatible
# with every Dart SDK in the matrix
RUN cd /usr/local/server/static-server && dart pub get

# Preinstall the Jaspr CLI so Jaspr sites build without declaring jaspr_cli as
# a project dependency, matching the Dockerfile Jaspr's own docs prescribe.
# `jaspr create` does not add it to pubspec.yaml, so `dart run jaspr_cli:jaspr`
# cannot resolve it from a stock project. PUB_CACHE/bin is already on PATH.
# Skipped below Dart 3.8, the CLI's minimum, which Flutter 3.24-3.29 predate.
RUN DART_VERSION="$(dart --version 2>&1 | sed -E 's/.*version: ([0-9]+\.[0-9]+\.[0-9]+).*/\1/')" \
    && if [ "$(printf '%s\n3.8.0\n' "$DART_VERSION" | sort -V | head -n1)" = "3.8.0" ]; then \
           dart pub global activate jaspr_cli; \
       else \
           echo "Skipping jaspr_cli: Dart ${DART_VERSION} is below the required 3.8.0"; \
       fi
