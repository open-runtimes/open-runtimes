LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk add util-linux isa-l
    else
        apt install -y util-linux isal
    fi
EOR

RUN mkdir -p /mnt/code
RUN mkdir -p /mnt/logs
RUN mkdir -p /mnt/telemetry
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY . .
