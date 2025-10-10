#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function

source /usr/local/server/helpers/remix/env.sh

cp ../server-remix.mjs ./server.mjs
mkdir -p ./ssr
cp -R ../ssr/* ./ssr/

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    START_COMMAND="node ./server.mjs"
else
    START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 PORT=3000 $START_COMMAND
