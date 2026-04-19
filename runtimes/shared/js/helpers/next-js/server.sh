#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function

source /usr/local/server/helpers/next-js/env.sh
source /usr/local/server/helpers/js-runner.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	# Detect SSR in custom output directory

	ENTRYPOINT="./server.js"
	if [ -e "$ENTRYPOINT" ]; then
		# Standalone approach
		START_COMMAND="$OPR_JS_RUNNER $ENTRYPOINT"
	else
		# Middleware approach
		cp ../server-next-js.mjs ./server.mjs
		START_COMMAND="$OPR_JS_RUNNER ./server.mjs"
	fi

else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
