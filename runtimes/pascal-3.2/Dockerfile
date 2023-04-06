FROM freepascal/fpc:3.2.2-focal-full

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src \
  && mkdir -p /usr/code \
  && mkdir -p /usr/workspace \
  && mkdir -p /usr/builtCode

WORKDIR /usr/local/src
COPY . .

RUN chmod +x /usr/local/src/build.sh \
  && chmod +x /usr/local/src/start.sh

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]
