#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo -e "\e[90m$(date '+%H:%M:%S') \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation started. \e[0m"

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
	echo -e "\e[90m$(date '+%H:%M:%S') \e[31m[\e[0mopen-runtimes\e[31m]\e[31m Error: No source code found. Ensure your source isn't empty. \e[0m"
	exit 1
fi

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

. /usr/local/server/helpers/prepare-build.sh

echo -e "\e[90m$(date '+%H:%M:%S') \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Environment preparation finished. \e[0m"

# Enter build folder
cd /usr/local/build

echo -e "\e[90m$(date '+%H:%M:%S') \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Build command execution started. \e[0m"
