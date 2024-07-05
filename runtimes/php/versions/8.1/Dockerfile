# syntax = devthefuture/dockerfile-x

FROM php:8.1-cli-alpine3.19 as step0
INCLUDE ./build

FROM php:8.1-cli-alpine3.19 as final
INCLUDE ./base-before
INCLUDE ./php
INCLUDE ./base-after
