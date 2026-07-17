#!/bin/bash
# Start lifecycle: extract -> start-prepare hook -> serve (with startup
# telemetry). Per-runtime steps live in /usr/local/server/hooks/.
# Fail if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/lifecycle/lib.sh

# Wall-clock + uptime anchor pair: every other value in timings.txt is an
# uptime-based delta, which is unmappable to the controller's timeline on its
# own. The pair lets the controller convert any uptime reading to wall time.
# Anchor keys are absolute stamps, never durations — the controller excludes
# them from histograms.
# Capture both stamps back-to-back before writing — the file appends can
# stall on disk I/O and would skew the pair.
anchor_wall=$(date +%s.%N)
anchor_uptime=$(opr_uptime)
[[ "$anchor_wall" == *N* ]] && anchor_wall=$(date +%s) # busybox date without %N
echo "anchor_wall=$anchor_wall" >>/mnt/telemetry/timings.txt
echo "anchor_uptime=$anchor_uptime" >>/mnt/telemetry/timings.txt

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

# Capture start time for startup metric. The anchor stamp marks the exact
# server-process exec point on the shared uptime timeline, so the controller
# can compute exec → runtime-main against the process's own timeOrigin.
start_uptime=$(opr_uptime)
export start_uptime
echo "anchor_server_start_uptime=$start_uptime" >>/mnt/telemetry/timings.txt

# Run server and monitor stdout for ready message
bash -c "$1" 2>&1 | {
	recorded=false
	while IFS= read -r line; do
		printf '%s\n' "$line"
		if [ "$recorded" = false ] && [[ "$line" == *"HTTP server successfully started"* || "$line" == *"server started on http://"* ]]; then
			end_uptime=$(awk '{print $1}' /proc/uptime)
			elapsed=$(awk "BEGIN{printf \"%.3f\", $end_uptime - $start_uptime}")
			echo "startup=$elapsed" >>/mnt/telemetry/timings.txt
			recorded=true
		fi
	done
}
