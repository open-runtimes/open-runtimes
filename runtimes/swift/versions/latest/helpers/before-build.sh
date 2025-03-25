#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

echo 'Building ...'