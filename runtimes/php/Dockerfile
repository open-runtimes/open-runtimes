# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
# Builder stage uses the same base unless a version pins a different one (8.3)
ARG BUILD_BASE_IMAGE=${BASE_IMAGE}

FROM ${BUILD_BASE_IMAGE} AS step0

# Extra packages only needed to compile extensions (linux-headers on php >= 8.2)
ARG BUILD_PACKAGES=""
RUN if [ -n "$BUILD_PACKAGES" ]; then apk add --no-cache $BUILD_PACKAGES; fi

ARG PHP_SWOOLE_VERSION
ENV PHP_SWOOLE_VERSION=$PHP_SWOOLE_VERSION
INCLUDE ./runtimes/php/build

# Stage compiled extensions at a fixed path so the final stage doesn't need to
# know the PHP-ABI-versioned extension directory name.
RUN mkdir -p /opt/ext \
    && cp "$(php-config --extension-dir)/swoole.so" /opt/ext/ \
    && (cp "$(php-config --extension-dir)"/yasd.so* /opt/ext/ 2>/dev/null || true)

FROM ${BASE_IMAGE} AS final
INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=index.php

RUN \
  apk add --no-cache --virtual .deps \
  make \
  automake \
  autoconf \
  gcc \
  g++ \
  curl-dev \
  && apk add --no-cache \
  libstdc++ \
  certbot \
  brotli-dev \
  yaml-dev \
  libmaxminddb-dev \
  libgomp \
  bash \
  && docker-php-ext-install -j"$(nproc)" opcache \
  && apk del .deps \
  && rm -rf /var/cache/apk/*

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN composer update --no-interaction --ignore-platform-reqs --optimize-autoloader --prefer-dist --no-dev
RUN mv vendor vendor-server

COPY --from=step0 /opt/ext/ /opt/ext/
RUN cp /opt/ext/* "$(php-config --extension-dir)/" && rm -rf /opt/ext

# Enable Extensions
RUN echo extension=swoole.so >> /usr/local/etc/php/conf.d/swoole.ini

ENV OPEN_RUNTIMES_SERVER_COMMAND="php -d memory_limit=8G src/server.php"

INCLUDE ./docker/base-after
