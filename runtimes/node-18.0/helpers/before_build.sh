#!/bin/sh
echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /usr/code/* /usr/builds

# Enter build folder
cd /usr/builds

echo 'Building ...'