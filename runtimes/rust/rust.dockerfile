ENV OPEN_RUNTIMES_ENTRYPOINT=main.rs

RUN apk update && apk add --no-cache \
    bash \
    musl-dev \
    openssl-dev \
    openssl-libs-static \
    pkgconfig \
    curl

RUN rustup component add rustfmt

COPY ./helpers ./helpers
