# syntax = devthefuture/dockerfile-x

FROM php:8.0-cli-alpine3.16 as step0
ARG PHP_SWOOLE_VERSION=v4.7.0
ENV PHP_SWOOLE_VERSION=$PHP_SWOOLE_VERSION
INCLUDE ./build

FROM php:8.0-cli-alpine3.16 as final
INCLUDE ./base-before
INCLUDE ./php
COPY --from=step0 /usr/local/lib/php/extensions/no-debug-non-zts-20200930/swoole.so /usr/local/lib/php/extensions/no-debug-non-zts-20200930/yasd.so* /usr/local/lib/php/extensions/no-debug-non-zts-20200930/
INCLUDE ./base-after
