#!/bin/bash
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/index.mjs"
if [ -e "$ENTRYPOINT" ]; then
    mv /usr/local/build/package*.json ./
    mv /usr/local/build/node_modules/ ./node_modules/
    rm -rf ./server/node_modules
fi
