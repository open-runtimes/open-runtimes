#!/bin/bash
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/server.mjs"
if [ -e "$ENTRYPOINT" ]; then
    echo "Bundling with server-side rendering support ..."
    mv /usr/local/build/package*.json ./
    mv /usr/local/build/node_modules/ ./node_modules/
fi
