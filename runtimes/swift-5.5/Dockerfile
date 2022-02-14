FROM swiftarm/swift:5.5.2-focal-multi-arch

LABEL maintainer="team@appwrite.io"

RUN groupadd -g 2000 open-runtimes \
&& useradd -m -u 2001 -g open-runtimes open-runtimes

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builtCode

WORKDIR /usr/local/src
COPY . .

RUN ["chmod", "+x", "/usr/local/src/start.sh"]
RUN ["chmod", "+x", "/usr/local/src/build.sh"]
RUN ["chmod", "+x", "/usr/local/src/deploy.sh"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/local/src"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/code"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/workspace"]
RUN ["chown", "-R", "open-runtimes:open-runtimes", "/usr/builtCode"]

USER open-runtimes

EXPOSE 3000

CMD ["/usr/local/src/start.sh"]