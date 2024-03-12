FROM golang:1.19-alpine

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src
COPY . .

RUN chmod +x ./build.sh
RUN chmod +x ./start.sh

EXPOSE 3000

CMD ["usr/local/src/start.sh"]
