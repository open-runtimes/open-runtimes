#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for start ..."

# Extract gzipped code from mounted volume to function folder
tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function

# Enter server folder
cd /usr/local/server

. /usr/local/server/helpers/prepare-start.sh

echo 'Starting ...'