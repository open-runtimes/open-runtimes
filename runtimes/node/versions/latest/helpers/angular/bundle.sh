#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/server.mjs"
if [ -e "$ENTRYPOINT" ]; then
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR started. [0m"

    mv /usr/local/build/package*.json ./
    mv /usr/local/build/node_modules/ ./node_modules/

    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR finished. [0m"
fi
