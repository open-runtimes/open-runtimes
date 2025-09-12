# syntax = devthefuture/dockerfile-x:1.4.2
FROM ghcr.io/cirruslabs/flutter:3.35.3

INCLUDE ./base-before
INCLUDE ./flutter
INCLUDE ./base-after

# Enable web platform
RUN flutter config --enable-web