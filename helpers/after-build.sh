#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Build command execution finished. \e[0m"

# Install dependencies
cd /usr/local/server

. /usr/local/server/helpers/prepare-compile.sh

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Build packaging started. \e[0m"

. /usr/local/server/helpers/prepare-packing.sh

# Finish build by preparing tar to use for starting the runtime
cd /usr/local/build/

# Check if the output directory is empty
# shellcheck disable=SC2086 # Intentional: unquoted variable allows ls -A to work when variable is empty
if [ -z "$(ls -A $OPEN_RUNTIMES_OUTPUT_DIRECTORY 2>/dev/null)" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[31m Error: No build output found. Ensure your output directory isn't empty. \e[0m"
	exit 1
fi

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Store entrypoint into build. Will be used during start process
touch .open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" >.open-runtimes

if [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "none" ]; then
	tar --exclude code.tar -cf /mnt/code/code.tar .
else
	# Default to gzip
	tar --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .
fi

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Build packaging finished. \e[0m"

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Build finished. \e[0m"
