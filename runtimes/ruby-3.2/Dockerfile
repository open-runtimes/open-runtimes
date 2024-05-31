FROM ruby:3.2-alpine3.19

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENTRYPOINT=index.rb

RUN mkdir -p /mnt/code
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

RUN apk add --update alpine-sdk

RUN gem install bundler

COPY . .

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

EXPOSE 3000
