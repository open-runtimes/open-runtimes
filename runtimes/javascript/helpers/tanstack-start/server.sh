#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/tanstack-start/env.sh
source /usr/local/server/helpers/javascript-runner/env.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	# Detect SSR in custom output directory
	ENTRYPOINT="./server/index.mjs"
	if [ -e "$ENTRYPOINT" ]; then
		# NitroV2, NitroV3. env.sh sets NITRO_PRESET=node_server, but a user's
		# vite.config can override it, so read the actual preset from nitro.json:
		# node-listener exports a handler and never listens (needs a wrapper),
		# anything else is a standalone self-listening server.
		PRESET=""
		if [ -e "./nitro.json" ]; then
			PRESET="$(sed -n 's/.*"preset"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' ./nitro.json)"
		fi

		if [ "$PRESET" = "node-listener" ]; then
			cp ../server-tanstack-start-listener.mjs ./server-http.mjs
			START_COMMAND="$OPR_JAVASCRIPT_RUNNER ./server-http.mjs"
		else
			START_COMMAND="$OPR_JAVASCRIPT_RUNNER ./server/index.mjs"
		fi
	fi

	ENTRYPOINT="./server/server.js"
	if [ -e "$ENTRYPOINT" ]; then
		# Native SSR (middleware)
		cp ../server-tanstack-start.mjs ./server-http.mjs
		START_COMMAND="$OPR_JAVASCRIPT_RUNNER ./server-http.mjs"
	fi

	# Realistically never happens - build prevents this
	if [ -z "$START_COMMAND" ]; then
		echo 'No server found'
		exit 1
	fi
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
