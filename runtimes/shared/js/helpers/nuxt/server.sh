#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/nuxt/env.sh
source /usr/local/server/helpers/js-runner.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	# Middleware-style
	cp ../server-nuxt.mjs ./server.mjs
	START_COMMAND="$OPR_JS_RUNNER ./server.mjs"

	# Standalone-style
	# START_COMMAND="$OPR_JS_RUNNER ./server/index.mjs"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
