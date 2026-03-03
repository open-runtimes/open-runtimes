LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk add --no-cache util-linux zstd
    else
        apt-get update && apt-get install -y util-linux zstd
    fi
EOR

RUN mkdir -p /mnt/code /mnt/logs /mnt/telemetry /usr/local/build /usr/local/server/src/function

WORKDIR /usr/local/server

COPY . .
