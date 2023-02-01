#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Prepare Deno cache folder
mkdir -p /usr/builds/deno-cache

# Set Deno Cache directory
export DENO_DIR="/usr/builds/deno-cache"

# Cache Server Depdenencies
cd /usr/local/src/
deno cache server.ts

# Cache user function depdenencies
cd /usr/builds

INSTALL_COMMAND=${1:-'deno cache $INTERNAL_RUNTIME_ENTRYPOINT'}
BUILD_COMMAND=${2:-''}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .