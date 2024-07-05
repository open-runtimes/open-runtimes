# syntax = devthefuture/dockerfile-x

FROM php:8.0-cli-alpine3.16 as step0
INCLUDE ./build

FROM php:8.0-cli-alpine3.16 as final
INCLUDE ./base-before
INCLUDE ./php
INCLUDE ./base-after
