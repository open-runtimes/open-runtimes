#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function

source /usr/local/server/helpers/remix/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    # Middleware-style
    cp ../server-remix.mjs ./server.mjs
    START_COMMAND="node ./server.mjs"
    
    # Standalone-style
    # Remix-serve requires "build" folder (might not work due to build folder structure)
    # mkdir -p build
    # mv server build/server
    # mv client build/client
    # START_COMMAND="./node_modules/.bin/remix-serve ./build/server/index.js"
else
    START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' HOST=0.0.0.0 PORT=3000 $START_COMMAND
