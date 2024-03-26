#!/bin/bash
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Create virtual env
cd /usr/local/build
python3 -m venv runtime-env
source runtime-env/bin/activate

# Enter build folder
cd /usr/local/build

echo 'Building ...'