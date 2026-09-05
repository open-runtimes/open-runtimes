#!/bin/bash
# Own the runtime process group so signals reach shell wrappers and workers too.
# setsid is supplied by util-linux in every runtime image.
set -u

shutdown_timeout=${OPEN_RUNTIMES_SHUTDOWN_TIMEOUT:-30}
if ! [[ "$shutdown_timeout" =~ ^[1-9][0-9]{0,4}$ ]] || [ "$shutdown_timeout" -gt 86400 ]; then
	echo 'OPEN_RUNTIMES_SHUTDOWN_TIMEOUT must be an integer between 1 and 86400 seconds' >&2
	exit 1
fi
export OPEN_RUNTIMES_SHUTDOWN_TIMEOUT="$shutdown_timeout"

child=
watchdog=
stopping=false
stop() {
	if [ "$stopping" = true ]; then return; fi
	stopping=true
	if [ -n "$child" ]; then
		# Use TERM for both TERM and INT: e.g. Gunicorn treats INT as a quick exit.
		kill -TERM -- "-$child" 2>/dev/null || kill -TERM "$child" 2>/dev/null || true
		setsid bash -c 'sleep "$1"; kill -KILL -- "-$2" 2>/dev/null || true' _ "$shutdown_timeout" "$child" &
		watchdog=$!
	fi
}
trap stop TERM INT

setsid "$@" &
child=$!
# A signal can arrive between starting the child and assigning its PID.
if [ "$stopping" = true ]; then
	stopping=false
	stop
fi

# A trapped signal interrupts wait; wait again to collect the actual exit code.
while true; do
	wait "$child"
	status=$?
	if ! kill -0 "$child" 2>/dev/null; then break; fi
done

if [ -n "$watchdog" ]; then
	kill -TERM -- "-$watchdog" 2>/dev/null || true
	wait "$watchdog" 2>/dev/null || true
fi
exit "$status"
