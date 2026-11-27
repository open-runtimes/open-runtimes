#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

# A Flask app runs under the runtime's gunicorn, wrapped in the Open Runtimes
# contract (secret, per-execution logs, timeout) by src/ssr/open_runtimes_wsgi.py.
# One worker per CPU and the app loaded before they fork, as for the function
# server; gunicorn's own timeout is off, so the execution timeout decides.
cpu_cores=$(echo "$OPEN_RUNTIMES_CPUS" | awk '{print int($1 + 0.999999)}')
workers=$((1 * cpu_cores))
if [ "$workers" -eq 0 ]; then
	workers=1
fi

if [ -z "$OPEN_RUNTIMES_START_COMMAND" ]; then
	START_COMMAND="python3 -m gunicorn --bind 0.0.0.0:3000 --workers $workers --timeout 0 --preload --pythonpath /usr/local/server/src/ssr open_runtimes_wsgi:application"
else
	START_COMMAND="$OPEN_RUNTIMES_START_COMMAND"
fi

echo "HTTP server successfully started!"
OPEN_RUNTIMES_WSGI=flask \
	HOST=0.0.0.0 \
	PORT=3000 \
	exec $START_COMMAND
