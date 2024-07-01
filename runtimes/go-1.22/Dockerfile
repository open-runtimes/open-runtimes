FROM golang:1.22-alpine 

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENTRYPOINT=main.go
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

RUN apk add git

RUN mkdir -p /mnt/code
RUN mkdir -p /mnt/logs
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY ./helpers ./helpers

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

COPY . .

EXPOSE 3000
