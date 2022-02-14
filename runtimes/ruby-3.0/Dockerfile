FROM ruby:3.0.3-alpine3.15

LABEL maintainer="team@appwrite.io"

RUN addgroup -S appwrite -g 2000 && adduser -S appwrite -G appwrite -D -u 2001

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builtCode

WORKDIR /usr/local/src/

RUN apk add --update alpine-sdk

RUN gem install bundler

COPY . .

EXPOSE 3000

RUN ["chmod", "+x", "/usr/local/src/start.sh"]
RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chown", "-R", "appwrite:appwrite", "/usr/local/src"]
RUN ["chown", "-R", "appwrite:appwrite", "/usr/code"]
RUN ["chown", "-R", "appwrite:appwrite", "/usr/workspace"]
RUN ["chown", "-R", "appwrite:appwrite", "/usr/builtCode"]
RUN ["chmod", "+w", "/tmp"]


USER appwrite
CMD [ "/usr/local/src/start.sh" ]