#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for start ..."

# Extract gzipped code from mounted volume to function folder
tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function

# Apply env vars from build step
set -o allexport
. /usr/local/server/src/function/.open-runtimes
set +o allexport

# Enter server folder
cd /usr/local/server

echo 'Starting ...'