ENV OPEN_RUNTIMES_ENTRYPOINT=index.cc
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

EXPOSE 3000
