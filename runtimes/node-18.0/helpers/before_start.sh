#!/bin/sh
echo "Preparing for start ..."

# Copy from mounted volume to temporary folder
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 

# Extract build from gzip file
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start

# Remove gzipped file
rm /usr/workspace/code.tar.gz

# Merge user's node_modules into server's node_modules
cp -R /usr/code-start/node_modules/* /usr/local/src/node_modules

# Enter server folder
cd /usr/local/src

echo 'Starting ...'