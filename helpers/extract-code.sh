#!/bin/bash
# Shared code extraction logic with sidecar support
# Source this script from before-start.sh to handle code extraction

# Prepare telemetry
mkdir -p /mnt/telemetry

# Check if code is pre-extracted (e.g., by sidecar)
if [ -f "/mnt/code/.extracted" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code already extracted, skipping extraction. \e[0m"

	start=$(awk '{print $1}' /proc/uptime)
	shopt -s dotglob
	symlink_failed=false
	for item in /mnt/code/*; do
		# Skip archive files and marker
		case "$(basename "$item")" in
		code.tar | code.tar.gz | code.gz | .extracted) continue ;;
		esac
		ln -s "$item" /usr/local/server/src/function/ || symlink_failed=true
	done
	shopt -u dotglob

	if [ "$symlink_failed" = true ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Symlink failed, falling back to extraction. \e[0m"
		rm -f /usr/local/server/src/function/*
		if [ -f /mnt/code/code.tar ]; then
			tar -xf /mnt/code/code.tar -C /usr/local/server/src/function
		elif [ -f /mnt/code/code.tar.gz ] || [ -f /mnt/code/code.gz ]; then
			tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function
		else
			echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code archive not found. \e[0m"
			exit 1
		fi
	fi

	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	if [ "$symlink_failed" = true ]; then
		echo "extract=$elapsed" >>/mnt/telemetry/timings.txt
	else
		echo "symlink=$elapsed" >>/mnt/telemetry/timings.txt
	fi
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

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction finished. \e[0m"

# Apply env vars from build step
set -o allexport
. /usr/local/server/src/function/.open-runtimes
set +o allexport
