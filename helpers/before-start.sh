#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepare telemetry
mkdir -p /mnt/telemetry

# Check if code is pre-extracted (e.g., by sidecar)
if [ -f "/mnt/code/.extracted" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code already extracted, skipping extraction. \e[0m"
	echo "extract=0.000" >>/mnt/telemetry/timings.txt
else
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction started. \e[0m"

	# Extract code from mounted volume to function folder
	start=$(awk '{print $1}' /proc/uptime)
	if [ -f /mnt/code/code.tar ]; then
		tar -xf /mnt/code/code.tar -C /usr/local/server/src/function
	elif [ -f /mnt/code/code.tar.gz ] || [ -f /mnt/code/code.gz ]; then
		tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function
	else
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code archive not found. \e[0m"
		exit 1
	fi

	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	echo "extract=$elapsed" >>/mnt/telemetry/timings.txt
fi

# Apply env vars from build step
set -o allexport
source /usr/local/server/src/function/.open-runtimes
set +o allexport

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction finished. \e[0m"

# Enter server folder
cd /usr/local/server

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation started. \e[0m"

. /usr/local/server/helpers/prepare-start.sh

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation finished. \e[0m"

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Runtime started. \e[0m"
