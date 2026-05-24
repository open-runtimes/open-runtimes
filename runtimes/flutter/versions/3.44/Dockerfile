# syntax = devthefuture/dockerfile-x:1.4.2
FROM ghcr.io/adrianjagielak/flutter:3.44.0

INCLUDE ./base-before
INCLUDE ./flutter
INCLUDE ./base-after

# Enable web platform
RUN flutter config --enable-web