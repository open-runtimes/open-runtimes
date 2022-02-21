FROM ruby:3.1-alpine3.15

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

RUN apk add --update alpine-sdk

RUN gem install bundler

COPY . .

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]