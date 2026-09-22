# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

# Extra system packages installed before everything else (java >= 22: curl)
ARG SYSTEM_PACKAGES=""
RUN if [ -n "$SYSTEM_PACKAGES" ]; then \
        if [ -f /etc/alpine-release ]; then \
            apk add --no-cache $SYSTEM_PACKAGES; \
        else \
            apt-get update && apt-get install -y $SYSTEM_PACKAGES; \
        fi; \
    fi

INCLUDE ./docker/base-before

RUN if [ -f /etc/alpine-release ]; then \
        apk add --no-cache zip bash; \
    else \
        apt-get update && apt-get install -y zip bash; \
    fi

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.java
ENV OPEN_RUNTIMES_SERVER_COMMAND="java -jar src/function/java-runtime-1.0.0.jar"

INCLUDE ./docker/base-after
