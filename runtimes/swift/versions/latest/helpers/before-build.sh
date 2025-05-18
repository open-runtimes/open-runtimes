#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for build ..."

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][31m Error: No source code found. Ensure your source isn't empty. [0m"
    exit 1
fi

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

echo 'Building ...'