# syntax=docker/dockerfile:1
# INCLUDE directives below are resolved by `bun ci/bake.ts`, which inlines the
# referenced *.dockerfile fragments into docker-bake.json (dockerfile-inline).
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk add --no-cache bash

# Maven resolves pom.xml dependencies during the build; the tarball is verified
# and used directly so no second JDK is pulled in by a distro maven package
ADD --checksum=sha256:80ffca22aed9e8b9713a232f3394fd81d7f20322df75efdb2b047dbd3e3a23bb https://repo.maven.apache.org/maven2/org/apache/maven/apache-maven/3.9.16/apache-maven-3.9.16-bin.tar.gz /tmp/maven.tar.gz
RUN tar -xzf /tmp/maven.tar.gz -C /opt && \
    ln -s /opt/apache-maven-3.9.16/bin/mvn /usr/local/bin/mvn && \
    rm /tmp/maven.tar.gz

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.kt

ENV OPEN_RUNTIMES_SERVER_COMMAND="java -jar src/function/kotlin-runtime-1.0.0.jar"

INCLUDE ./docker/base-after
