FROM alpine:3.16

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ARG DEBIAN_FRONTEND=noninteractive
ENV DROGON_VERSION=v1.7.5

RUN apk add --no-cache \
    git \
    gcc \
    g++ \
    cmake \
    make \
    openssl \
    doxygen \
    jsoncpp-dev \
    openssl-dev \
    zlib-dev \
    curl-dev \
    util-linux-dev

RUN git clone --depth 1 --branch $DROGON_VERSION https://github.com/drogonframework/drogon \
    && cd drogon \
    && git submodule update --init \
    && mkdir build \
    && cd build \
    && cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make -j"$(nproc)" \
    && make install \
    && cd / \
    && rm -rf /drogon

RUN mkdir -p /mnt/code
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY . .

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

EXPOSE 3000
