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

# Enter server folder
cd /usr/local/server

prepare_start=$(awk '{print $1}' /proc/uptime)
. /usr/local/server/helpers/prepare-start.sh
prepare_end=$(awk '{print $1}' /proc/uptime)
prepare_elapsed=$(awk "BEGIN{printf \"%.3f\", $prepare_end - $prepare_start}")
echo "prepare=$prepare_elapsed" >>/mnt/telemetry/timings.txt

echo 'Starting ...'
