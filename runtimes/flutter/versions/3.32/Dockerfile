# syntax = devthefuture/dockerfile-x:1.4.2
FROM ghcr.io/cirruslabs/flutter:3.32.0

INCLUDE ./base-before
INCLUDE ./flutter
INCLUDE ./base-after

# Enable web platform
RUN flutter config --enable-web