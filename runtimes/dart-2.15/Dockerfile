FROM dart:2.15

LABEL maintainer="team@appwrite.io"

RUN groupadd -g 2000 open-runtimes \
&& useradd -m -u 2001 -g open-runtimes open-runtimes

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src
COPY . /usr/local/src

RUN ["chmod", "+x", "/usr/local/src/start.sh"]
RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/local/src"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/code"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/workspace"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/builds"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/lib/dart"]

RUN dart --disable-analytics

USER open-runtimes

CMD ["tail", "-f", "/dev/null"]
