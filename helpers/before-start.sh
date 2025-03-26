#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "[37mPreparing for start ... [0m"

# Extract gzipped code from mounted volume to function folder
tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function

# Apply env vars from build step
set -o allexport
source /usr/local/server/src/function/.open-runtimes
set +o allexport

# Enter server folder
cd /usr/local/server

. /usr/local/server/helpers/prepare-start.sh

echo "[37mStarting ... [0m"