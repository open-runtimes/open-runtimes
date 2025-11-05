#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/fresh/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	# Standalone-style
	START_COMMAND="deno run --allow-env --allow-net --allow-read --import /usr/local/server/src/ssr/injections.mjs ./compiled-entry.js"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
	# TODO: When supporting standalone Deno, add enforced injection of --import
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
