#!/bin/bash
# Start lifecycle: extract -> start-prepare hook -> serve (with startup
# telemetry). Per-runtime steps live in /usr/local/server/hooks/.
# Fail if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/lifecycle/lib.sh

# Extract code (handles sidecar pre-extraction)
. /usr/local/server/helpers/lifecycle/extract.sh

# Enter server folder
cd /usr/local/server

opr_log "Environment preparation started."

# Track environment preparation timing
prepare_start=$(opr_uptime)
opr_run_hook start-prepare
prepare_end=$(opr_uptime)
prepare_elapsed=$(awk "BEGIN{printf \"%.3f\", $prepare_end - $prepare_start}")
echo "prepare=$prepare_elapsed" >>/mnt/telemetry/timings.txt

opr_log "Environment preparation finished."

opr_success "Runtime started."

# Capture start time for startup metric
start_uptime=$(opr_uptime)
export start_uptime

# Keep the lifecycle alive until both the server and its log reader finish.
# The supervisor signals the whole group; the server handles TERM itself.
stopping=false
trap 'stopping=true' TERM INT
exec 3> >(
	# Keep draining output when the supervisor signals the runtime group.
	trap '' TERM INT
	recorded=false
	while IFS= read -r line || [ -n "$line" ]; do
		printf '%s\n' "$line"
		if [ "$recorded" = false ] && [[ "$line" == *"HTTP server successfully started"* || "$line" == *"server started on http://"* ]]; then
			end_uptime=$(awk '{print $1}' /proc/uptime)
			elapsed=$(awk "BEGIN{printf \"%.3f\", $end_uptime - $start_uptime}")
			echo "startup=$elapsed" >>/mnt/telemetry/timings.txt
			recorded=true
		fi
	done
)
log_reader=$!
bash -c "$1" >&3 2>&1 &
server=$!
# Cover TERM arriving while the log reader/server were being started.
if [ "$stopping" = true ]; then kill -TERM "$server" 2>/dev/null || true; fi
exec 3>&-

wait_for() {
	local pid=$1 status
	while true; do
		status=0
		wait "$pid" || status=$?
		if ! kill -0 "$pid" 2>/dev/null; then return "$status"; fi
	done
}
status=0
wait_for "$server" || status=$?
wait_for "$log_reader" || true
exit "$status"
