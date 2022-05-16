FROM alpine:3.15

LABEL maintainer="team@appwrite.io"

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

RUN mkdir -p /usr/local/src/build \
    && mkdir -p /usr/code \
    && mkdir -p /usr/workspace

WORKDIR /usr/local/src

COPY . .

RUN chmod +x /usr/local/src/start.sh \
    && chmod +x /usr/local/src/build.sh

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]
