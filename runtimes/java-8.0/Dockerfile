FROM openjdk:8-jdk-slim

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builtCode

WORKDIR /usr/local/src
COPY . .

RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chmod", "+x", "/usr/local/src/start.sh"]

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]