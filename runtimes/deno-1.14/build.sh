#!/bin/sh

# Prepare separate directory to prevent changign user's files
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
deno cache $ENTRYPOINT_NAME

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .