#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
    echo "ERROR: No source code found. Please ensure your source directory exists and isn't empty."
    exit 1
fi

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

echo 'Building ...'