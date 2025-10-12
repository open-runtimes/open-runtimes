# syntax = devthefuture/dockerfile-x:1.4.2

FROM php:8.2.29-cli-alpine3.20 as step0
RUN apk update && apk add --no-cache linux-headers
ARG PHP_SWOOLE_VERSION=v5.1.2
ENV PHP_SWOOLE_VERSION=$PHP_SWOOLE_VERSION
INCLUDE ./build

FROM php:8.2.24-cli-alpine3.20 as final
INCLUDE ./base-before
INCLUDE ./php
COPY --from=step0 /usr/local/lib/php/extensions/no-debug-non-zts-20220829/swoole.so /usr/local/lib/php/extensions/no-debug-non-zts-20220829/yasd.so* /usr/local/lib/php/extensions/no-debug-non-zts-20220829/
INCLUDE ./base-after
