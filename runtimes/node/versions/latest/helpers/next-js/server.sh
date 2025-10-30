#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function

source /usr/local/server/helpers/next-js/env.sh

WEBPACK_ENTRYPOINT="./server/webpack-runtime.js"
TURBOPACK_ENTRYPOINT="./turbopack"
STANDALONE_ENTRYPOINT="./server.js"

ls -al .

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
    # Detect SSR in custom output directory
	if [ -e "$STANDALONE_ENTRYPOINT" ]; then
		# Standalone
		START_COMMAND="node $STANDALONE_ENTRYPOINT"
	fi
    
	ENTRYPOINT="./server/server.js"
	if [ -e "$WEBPACK_ENTRYPOINT" ] || [ -e "$TURBOPACK_ENTRYPOINT" ]; then
		# No special export (middleware)
		cp ../server-next-js.mjs ./server.mjs
		START_COMMAND="node ./server.mjs"
	fi
    
	# Realistically never happens - build prevents this
	if [ -z "$START_COMMAND" ]; then
		echo 'No server found'
		exit 1
	fi
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"' \
	HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
