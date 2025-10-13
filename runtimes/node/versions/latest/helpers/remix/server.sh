#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function

source /usr/local/server/helpers/remix/env.sh

# Remix-serve requires "build" folder
mkdir -p build
mv server build/server
mv client build/client

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    START_COMMAND="./node_modules/.bin/remix-serve ./build/server/index.js"
else
    START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' HOST=0.0.0.0 PORT=3000 $START_COMMAND
