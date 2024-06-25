FROM python:3.11-bookworm

LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"


RUN apt-get update && apt-get install -y gcc pkg-config libhdf5-dev

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENTRYPOINT=main.py
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

RUN mkdir -p /mnt/code
RUN mkdir -p /mnt/logs
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY requirements.txt .
COPY . .

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

ENV FLASK_APP="/usr/local/server/src/server.py"

EXPOSE 3000
