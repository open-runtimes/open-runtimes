FROM php:8.1.6-alpine as step0

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
  libmaxminddb-dev

RUN docker-php-ext-install sockets

RUN \
  ## Swoole Extension
  git clone --depth 1 --branch $PHP_SWOOLE_VERSION https://github.com/swoole/swoole-src.git && \
  cd swoole-src && \
  phpize && \
  ./configure --enable-http2 && \
  make && make install && \
  cd ..

FROM php:8.1.6-alpine as final

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=index.php

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

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src/

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY --from=step0 /usr/local/lib/php/extensions/no-debug-non-zts-20210902/swoole.so /usr/local/lib/php/extensions/no-debug-non-zts-20210902/yasd.so* /usr/local/lib/php/extensions/no-debug-non-zts-20210902/

COPY composer* /usr/local/src/

# Add Source Code
COPY ./server.php /usr/local/src/
COPY ./start.sh /usr/local/src/
COPY ./build.sh /usr/local/src/
COPY ./prepare.php /usr/local/src/

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

# Enable Extensions
RUN echo extension=swoole.so >> /usr/local/etc/php/conf.d/swoole.ini

EXPOSE 3000

CMD [ "/usr/local/src/start.sh" ]
