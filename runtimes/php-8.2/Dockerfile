FROM php:8.2-alpine3.16 as step0

ENV PHP_SWOOLE_VERSION=v4.8.7

RUN \
  apk add --no-cache --virtual .deps \
  make \
  automake \
  autoconf \
  gcc \
  g++ \
  git \
  zlib-dev \
  brotli-dev \
  yaml-dev \
  libmaxminddb-dev \
  openssl-dev

RUN docker-php-ext-install sockets

RUN \
  ## Swoole Extension
  git clone --depth 1 --branch $PHP_SWOOLE_VERSION https://github.com/swoole/swoole-src.git && \
  cd swoole-src && \
  phpize && \
  ./configure --enable-http2 --enable-openssl && \
  make && make install && \
  cd ..

FROM php:8.2-alpine3.16 as final

LABEL maintainer="team@appwrite.io"

ENV OPEN_RUNTIMES_ENTRYPOINT=index.php

RUN \
  apk update \
  && apk add --no-cache --virtual .deps \
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
  && docker-php-ext-install opcache \
  && apk del .deps \
  && rm -rf /var/cache/apk/*

RUN mkdir -p /mnt/code
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY --from=step0 /usr/local/lib/php/extensions/no-debug-non-zts-20210902/swoole.so /usr/local/lib/php/extensions/no-debug-non-zts-20210902/yasd.so* /usr/local/lib/php/extensions/no-debug-non-zts-20210902/

# Add Source Code
COPY . .

RUN composer update --no-interaction --ignore-platform-reqs --optimize-autoloader --prefer-dist --no-dev
RUN mv vendor vendor-server

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

# Enable Extensions
RUN echo extension=swoole.so >> /usr/local/etc/php/conf.d/swoole.ini

EXPOSE 3000