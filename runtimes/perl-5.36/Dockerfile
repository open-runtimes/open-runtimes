FROM perl:5.36-slim

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=index.pl

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY . .

RUN apt-get update; apt-get install -y curl gcc build-essential
RUN cpanm Carton
RUN carton install

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]
