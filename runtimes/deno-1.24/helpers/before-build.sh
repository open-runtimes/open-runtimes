#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Prepare Deno cache folder
mkdir -p /usr/local/build/deno-cache

# Set Deno Cache directory
export DENO_DIR="/usr/local/build/deno-cache"

echo 'Building ...'

# Cache server dependencies
cd /usr/local/server/src
deno cache server.ts

# Enter build folder
cd /usr/local/build