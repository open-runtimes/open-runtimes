FROM swiftarm/swift:5.5.2-focal-multi-arch

LABEL maintainer="team@appwrite.io"

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace

WORKDIR /usr/local/src
COPY . .

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]