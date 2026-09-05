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

# Tini forwards signals to the whole process group. Keep the shell and log
# reader alive until the server finishes; the container manager owns the deadline.
# Bash defers this trap during the pipeline, but exits if signaled before launch.
trap 'exit "${PIPESTATUS[0]}"' TERM INT
bash -c "$1" 2>&1 | {
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
}

exit "${PIPESTATUS[0]}"
