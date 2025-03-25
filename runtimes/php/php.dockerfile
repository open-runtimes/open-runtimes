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
  bash \
  && docker-php-ext-install opcache \
  && apk del .deps \
  && rm -rf /var/cache/apk/*

RUN mkdir -p /mnt/code
RUN mkdir -p /mnt/logs
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

# Add Source Code
COPY . .

RUN composer update --no-interaction --ignore-platform-reqs --optimize-autoloader --prefer-dist --no-dev
RUN mv vendor vendor-server

# Enable Extensions
RUN echo extension=swoole.so >> /usr/local/etc/php/conf.d/swoole.ini
