FROM rust:1.55-alpine

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src
COPY . /usr/local/src

RUN apk add --no-cache musl-dev

RUN ["chmod", "+x", "/usr/local/src/start.sh"]
RUN ["chmod", "+x", "/usr/local/src/build.sh"]

CMD ["tail", "-f", "/dev/null"]