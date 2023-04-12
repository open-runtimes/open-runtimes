FROM python:3.10-slim-bullseye

LABEL maintainer="team@appwrite.io"

WORKDIR /tmp
RUN apt-get update
RUN apt-get install --yes curl
RUN curl -o anaconda.sh https://repo.anaconda.com/archive/Anaconda3-2023.03-Linux-x86_64.sh
RUN sha256sum anaconda.sh > hash.txt
RUN sha256sum -c hash.txt
RUN sh anaconda.sh -b -p $HOME/miniconda
RUN rm anaconda.sh

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