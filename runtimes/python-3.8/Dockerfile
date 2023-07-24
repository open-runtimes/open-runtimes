FROM python:3.8-alpine3.15

LABEL maintainer="team@appwrite.io"

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

RUN mkdir -p /mnt/code
RUN mkdir -p /usr/local/build
RUN mkdir -p /usr/local/server
RUN mkdir -p /usr/local/server/src
RUN mkdir -p /usr/local/server/src/function

WORKDIR /usr/local/server

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY . .

RUN chmod +x /usr/local/server/helpers/before-start.sh
RUN chmod +x /usr/local/server/helpers/start.sh

RUN chmod +x /usr/local/server/helpers/before-build.sh
RUN chmod +x /usr/local/server/helpers/build.sh
RUN chmod +x /usr/local/server/helpers/after-build.sh

ENV FLASK_APP="/usr/local/server/src/server.py"

EXPOSE 3000