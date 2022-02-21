FROM python:3.9-alpine3.15

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY . .

ENV FLASK_APP="/usr/local/src/server.py"

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]