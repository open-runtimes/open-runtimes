# syntax = devthefuture/dockerfile-x:1.4.2

FROM php:8.3.26-cli-alpine3.21 as step0
RUN apk update && apk add --no-cache linux-headers
ARG PHP_SWOOLE_VERSION=v5.1.2
ENV PHP_SWOOLE_VERSION=$PHP_SWOOLE_VERSION
INCLUDE ./build

FROM php:8.3.12-cli-alpine3.20 as final
INCLUDE ./base-before
INCLUDE ./php
COPY --from=step0 /usr/local/lib/php/extensions/no-debug-non-zts-20230831/swoole.so /usr/local/lib/php/extensions/no-debug-non-zts-20230831/yasd.so* /usr/local/lib/php/extensions/no-debug-non-zts-20230831/
INCLUDE ./base-after
