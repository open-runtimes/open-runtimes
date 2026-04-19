#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/angular/env.sh
source /usr/local/server/helpers/javascript-runner.sh

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	# Middleware-style
	cp ../server-angular.mjs ./server.mjs
	START_COMMAND="$OPR_JAVASCRIPT_RUNNER ./server.mjs"

	# Standalone-style
	# START_COMMAND="$OPR_JAVASCRIPT_RUNNER ./server/server.mjs"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
