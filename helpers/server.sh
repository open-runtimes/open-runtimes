#!/bin/bash
# Executor contract: started via `bash helpers/server.sh`.
# Runs the runtime's server command from OPEN_RUNTIMES_SERVER_COMMAND (set in
# each runtime's dockerfile). Runtimes that need more than a single command
# ship a hooks/server.sh instead, which takes precedence.
set -e
shopt -s dotglob

if [ -f /usr/local/server/hooks/server.sh ]; then
	. /usr/local/server/hooks/server.sh
else
	exec bash -c "$OPEN_RUNTIMES_SERVER_COMMAND"
fi
