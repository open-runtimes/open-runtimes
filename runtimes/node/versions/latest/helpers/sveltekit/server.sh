#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/sveltekit/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    START_COMMAND="node ./index.js"
else
    START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' HOST=0.0.0.0 PORT=3000 $START_COMMAND
