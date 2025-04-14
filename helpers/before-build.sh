#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Environment preparation started. [0m"

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
    echo "ERROR: No source code found. Ensure your source directory exists and isn't empty."
    exit 1
fi

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

. /usr/local/server/helpers/prepare-build.sh

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Environment preparation finished. [0m"

# Enter build folder
cd /usr/local/build

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Build command execution started. [0m"

