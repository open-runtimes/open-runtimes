#!/bin/bash
# Fail build if any command fails
set -e

echo "Preparing for start ..."

# Extract gzipped code from mounted volume to function folder
tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function

# Apply env vars from build step
set -o allexport
. /usr/local/server/src/function/.open-runtimes
set +o allexport

# Activate virtual env
. /usr/local/server/src/function/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"

# Enter server folder
cd /usr/local/server

echo 'Starting ...'
