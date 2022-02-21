FROM dart:2.15

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/local/src/user_code
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY . .

RUN dart --disable-analytics

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]