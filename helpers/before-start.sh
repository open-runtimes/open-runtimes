#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Extract code (handles sidecar pre-extraction)
. /usr/local/server/helpers/extract-code.sh

# Enter server folder
cd /usr/local/server

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation started. \e[0m"

# Track environment preparation timing
prepare_start=$(awk '{print $1}' /proc/uptime)
. /usr/local/server/helpers/prepare-start.sh
prepare_end=$(awk '{print $1}' /proc/uptime)
prepare_elapsed=$(awk "BEGIN{printf \"%.3f\", $prepare_end - $prepare_start}")
echo "prepare=$prepare_elapsed" >>/mnt/telemetry/timings.txt

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation finished. \e[0m"

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Runtime started. \e[0m"
