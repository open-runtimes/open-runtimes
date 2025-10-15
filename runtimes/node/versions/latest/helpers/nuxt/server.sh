#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/nuxt/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    # Middleware-style
    cp ../server-nuxt.mjs ./server.mjs
    START_COMMAND="node ./server.mjs"
   
    # Standalone-style
    # START_COMMAND="node ./server/index.mjs"
else
    START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' HOST=0.0.0.0 PORT=3000 $START_COMMAND
