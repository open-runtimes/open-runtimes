# syntax = devthefuture/dockerfile-x:1.4.2
FROM dart:3.5.2

# Install Flutter dependencies
RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    git \
    xz-utils \
    && apt-get clean

# Install Flutter
ENV PATH="/source/flutter/bin:$PATH"
RUN mkdir -p /source
RUN curl -o /source/stable.tar.xz "https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.24.5-stable.tar.xz"
RUN cd /source && tar -xf stable.tar.xz -C ./ && rm stable.tar.xz
RUN git config --global --add safe.directory /source/flutter

RUN flutter config --enable-web

INCLUDE ./base-before
INCLUDE ./dart
INCLUDE ./base-after

