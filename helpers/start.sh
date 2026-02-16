#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/before-start.sh

# Capture start time for startup metric
start_uptime=$(awk '{print $1}' /proc/uptime)

# Run server and monitor stdout for ready message
bash -c "$1" 2>&1 | {
    recorded=false
    while IFS= read -r line; do
        printf '%s\n' "$line"
        if [ "$recorded" = false ] && [ "$line" = "HTTP server successfully started!" ]; then
            end_uptime=$(awk '{print $1}' /proc/uptime)
            elapsed=$(awk "BEGIN{printf \"%.3f\", $end_uptime - $start_uptime}")
            echo "startup=$elapsed" >> /mnt/telemetry/timings.txt
            recorded=true
        fi
    done
}
