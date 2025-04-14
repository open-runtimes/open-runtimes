#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob


echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Build command execution finished. [0m"

# Install dependencies
cd /usr/local/server

. /usr/local/server/helpers/prepare-compile.sh

echo "[90m$(date +[%H:%M:%S]) [31m[[0mopen-runtimes[31m][97m Build packaging started. [0m"

. /usr/local/server/helpers/prepare-packing.sh

# Finish build by preparing tar to use for starting the runtime
cd /usr/local/build/
if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"  ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

# Check if the output directory is empty
if [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][31m Error: No build output found. Ensure your output directory isn't empty. [0m"
    exit 1
fi

# Store entrypoint into build. Will be used during start process
touch .open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > .open-runtimes

tar --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Build packaging finished. [0m"

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][32m Build finished. [0m"