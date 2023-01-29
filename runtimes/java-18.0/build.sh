#!/bin/sh

set -e

mkdir -p /usr/local/src/src/main/java/io/openruntimes/java
cp -a /usr/code/. /usr/local/src/src/main/java/io/openruntimes/java

cd /usr/local/src/src/main/java/io/openruntimes/java

# Apply all gradle files to the root project
for filename in ./*.gradle*; do
    if [ ! -f "${filename}" ]; then
        continue;
    fi
    mv "${filename}" "/usr/local/src/${filename}"
    echo "apply from: \"${filename}\"" >> /usr/local/src/build.gradle
done

# Read user code and collect imports
CODE="$(cat "$OPEN_RUNTIMES_ENTRYPOINT")"
IMPORTS=""
while read line; do
    case "${line}" in import*)
        IMPORTS="${IMPORTS}${line}\n"
    esac
done < "$OPEN_RUNTIMES_ENTRYPOINT"
CODE=$(echo "${CODE}" | sed /import*/d)

# Wrap the user code in a class
echo "package io.openruntimes.java;
${IMPORTS}
public class Wrapper {
${CODE}
}" > Wrapper.java

# Remove the user code file (copy)
rm "${OPEN_RUNTIMES_ENTRYPOINT}"

# Build the jar
cd /usr/local/src
sh gradlew buildJar

# Tar the jar
cd /usr/local/src/build/libs/
tar -zcvf /usr/code/code.tar.gz .