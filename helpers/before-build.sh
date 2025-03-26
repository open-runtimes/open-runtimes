#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob


echo "[37mPreparing for build ... [0m"

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

. /usr/local/server/helpers/prepare-build.sh

# Enter build folder
cd /usr/local/build

echo "[37mBuilding ... [0m"
