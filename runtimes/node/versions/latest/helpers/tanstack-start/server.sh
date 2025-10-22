#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/tanstack-start/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    # Detect SSR in custom output directory
    ENTRYPOINT="./server/index.mjs"
    if [ -e "$ENTRYPOINT" ]; then
        # NitroV2, NitroV3 (standalone)
        START_COMMAND="node ./server/index.mjs"
    fi
    
    ENTRYPOINT="./server/server.js"
    if [ -e "$ENTRYPOINT" ]; then
        # Native SSR (middleware)
        cp ../server-tanstack-start.mjs ./server-http.mjs
        START_COMMAND="node ./server-http.mjs"
    fi
    
    # Realistically never happens - build prevents this
    if [ -z "$START_COMMAND" ]; then
        echo 'No server found'
        exit 1
    fi
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' HOST=0.0.0.0 PORT=3000 $START_COMMAND
