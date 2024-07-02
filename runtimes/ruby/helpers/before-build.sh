#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Enter build folder
cd /usr/local/build

# Link user's depenrencies
if [ -f "Gemfile" ]; then
    echo "eval_gemfile '/usr/local/build/Gemfile'" >> /usr/local/server/Gemfile
fi

bundle config set --local path 'vendor/bundle'

# Enter build folder
cd /usr/local/build

echo 'Building ...'