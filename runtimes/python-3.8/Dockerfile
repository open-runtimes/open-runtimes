FROM python:3.8.12-alpine3.15

RUN addgroup -S open-runtimes -g 2000 && adduser -S open-runtimes -G open-runtimes -D -u 2001

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builtCode

WORKDIR /usr/local/src/
COPY . .

ENV FLASK_APP="/usr/local/src/server.py"

EXPOSE 3000

RUN ["chmod", "+x", "/usr/local/src/start.sh"]
RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/local/src"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/code"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/workspace"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/builtCode"]

USER open-runtimes

CMD [ "/usr/local/src/start.sh" ]
