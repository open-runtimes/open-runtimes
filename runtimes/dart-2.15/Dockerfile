FROM dart:2.15

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=lib/main.dart

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/local/src/user_code
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

COPY . .

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

RUN dart --disable-analytics

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]