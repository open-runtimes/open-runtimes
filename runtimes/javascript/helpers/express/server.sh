#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/javascript-runner/env.sh

# Drain active requests on SIGTERM, as the bundled SSR servers do
export NODE_OPTIONS="$NODE_OPTIONS --import /usr/local/server/helpers/express/shutdown.mjs"

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	START_COMMAND="$OPR_JAVASCRIPT_RUNNER $OPEN_RUNTIMES_ENTRYPOINT"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 \
	PORT=3000 \
	exec $START_COMMAND
