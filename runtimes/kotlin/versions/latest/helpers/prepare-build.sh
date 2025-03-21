#!/bin/bash
# Copy user code to server code
mkdir -p /usr/local/server/src/main/java/io/openruntimes/java
cp -a /usr/local/build/. /usr/local/server/src/main/java/io/openruntimes/java

# Link user's depenrencies
cd /usr/local/server/src/main/java/io/openruntimes/java
for filename in ./*.gradle*; do
    if [ ! -f "${filename}" ]; then
        continue;
    fi
    mv "${filename}" "/usr/local/server/${filename}"
    echo "apply from: \"${filename}\"" >> /usr/local/server/build.gradle
done
