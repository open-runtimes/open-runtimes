ENV OPEN_RUNTIMES_ENTRYPOINT=

RUN apt-get update && apt-get install -y --no-install-recommends zip bash binutils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*