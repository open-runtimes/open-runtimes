#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for start ..."

# Prepare telemetry
mkdir -p /mnt/telemetry

# Extract gzipped code from mounted volume to function folder
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

# Apply env vars from build step
set -o allexport
. /usr/local/server/src/function/.open-runtimes
set +o allexport

# Enter server folder
cd /usr/local/server

echo 'Starting ...'
