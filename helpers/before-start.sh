#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepare telemetry
mkdir -p /mnt/telemetry

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction started. \e[0m"

# Extract code from mounted volume to function folder
if [ -f /mnt/code/code.tar ]; then
	# Uncompressed tar - only one extraction needed
	start=$(awk '{print $1}' /proc/uptime)
	tar -xf /mnt/code/code.tar -C /usr/local/server/src/function
	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	echo "extract_tar=$elapsed" >>/mnt/telemetry/timings.txt
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Uncompressed tar extraction: ${elapsed}s \e[0m"

elif [ -f /mnt/code/code.tar.gz ] || [ -f /mnt/code/code.gz ]; then
	# Run regular gzip extraction first
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Running regular gzip extraction... \e[0m"
	start_regular=$(awk '{print $1}' /proc/uptime)
	tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function
	end_regular=$(awk '{print $1}' /proc/uptime)
	elapsed_regular=$(awk "BEGIN{printf \"%.3f\", $end_regular - $start_regular}")
	echo "extract_gzip=$elapsed_regular" >>/mnt/telemetry/timings.txt
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Regular gzip extraction: ${elapsed_regular}s \e[0m"

	# Clean up for pigz test
	rm -rf /usr/local/server/src/function/*

	# Run pigz extraction
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Running pigz extraction... \e[0m"
	start_pigz=$(awk '{print $1}' /proc/uptime)
	tar -I pigz -xf /mnt/code/code.tar.gz -C /usr/local/server/src/function
	end_pigz=$(awk '{print $1}' /proc/uptime)
	elapsed_pigz=$(awk "BEGIN{printf \"%.3f\", $end_pigz - $start_pigz}")
	echo "extract_pigz=$elapsed_pigz" >>/mnt/telemetry/timings.txt
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Pigz extraction: ${elapsed_pigz}s \e[0m"

	# Log comparison
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Extraction comparison - gzip: ${elapsed_regular}s vs pigz: ${elapsed_pigz}s \e[0m"

else
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code archive not found. \e[0m"
	exit 1
fi

# Apply env vars from build step
set -o allexport
source /usr/local/server/src/function/.open-runtimes
set +o allexport

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction finished. \e[0m"

# Enter server folder
cd /usr/local/server

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation started. \e[0m"

prepare_start=$(awk '{print $1}' /proc/uptime)
. /usr/local/server/helpers/prepare-start.sh
prepare_end=$(awk '{print $1}' /proc/uptime)
prepare_elapsed=$(awk "BEGIN{printf \"%.3f\", $prepare_end - $prepare_start}")
echo "prepare=$prepare_elapsed" >>/mnt/telemetry/timings.txt

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation finished. \e[0m"

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Runtime started. \e[0m"
