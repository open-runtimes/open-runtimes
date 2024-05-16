#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for start ..."

# Extract gzipped code from mounted volume to function folder
tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function

# Apply env vars from build step
set -o allexport
source /usr/local/server/src/function/.open-runtimes
set +o allexport

# Enter server folder
cd /usr/local/server

# Link user's depenrencies
if [ -f "/usr/local/server/src/function/Gemfile" ]; then
    echo "eval_gemfile '/usr/local/server/src/function/Gemfile'" >> /usr/local/server/Gemfile
fi

# Copy dependencies
cp -R /usr/local/server/src/function/vendor /usr/local/server/vendor

bundle config set --local path 'vendor/bundle'

echo 'Starting ...'