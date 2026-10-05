#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	chmod +x ./app
	START_COMMAND="./app"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

HOST=0.0.0.0 \
	PORT=3000 \
	$START_COMMAND
