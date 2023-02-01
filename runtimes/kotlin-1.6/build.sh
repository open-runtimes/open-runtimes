#!/bin/sh

set -e

mkdir -p /usr/local/src/src/main/kotlin/io/openruntimes/kotlin
cp -a /usr/code/. /usr/local/src/src/main/kotlin/io/openruntimes/kotlin

cd /usr/local/src/src/main/kotlin/io/openruntimes/kotlin
if [[ ! -f "deps.gradle" ]]; then
    mv /usr/local/src/deps.gradle.fallback /usr/local/src/src/main/kotlin/io/openruntimes/kotlin/deps.gradle
fi

# Apply all gradle files to the root project
for filename in ./*.gradle*; do
    if [ ! -f "${filename}" ]; then
        continue;
    fi
    mv "${filename}" "/usr/local/src/${filename}"
    echo "apply from: \"${filename}\"" >> /usr/local/src/build.gradle
done

# Read user code and collect imports
CODE="$(cat "$INTERNAL_RUNTIME_ENTRYPOINT")"
IMPORTS=""
while read line; do
    case "${line}" in import*)
        IMPORTS="${IMPORTS}${line}\n"
    esac
done < "$INTERNAL_RUNTIME_ENTRYPOINT"
CODE=$(echo "${CODE}" | sed /import*/d)

# Wrap the user code in a class
echo "package io.openruntimes.kotlin
${IMPORTS}
class Wrapper {
${CODE}
}" > Wrapper.kt

# Remove the user code file (copy)
rm "${INTERNAL_RUNTIME_ENTRYPOINT}"

# Build the jar
cd /usr/local/src

INSTALL_COMMAND=${1:-''}
BUILD_COMMAND=${2:-'sh gradlew buildJar'}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

# Tar the jar
cd /usr/local/src/build/libs/
tar -zcvf /usr/code/code.tar.gz .