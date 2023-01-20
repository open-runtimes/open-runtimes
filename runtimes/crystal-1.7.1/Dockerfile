FROM crystallang/crystal:1.7.1-alpine

LABEL maintainer="chris@watzon.tech"

ENV INTERNAL_RUNTIME_ENTRYPOINT=src/main.cr

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/local/src/user_code
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY . .

RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chmod", "+x", "/usr/local/src/start.sh"]

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]