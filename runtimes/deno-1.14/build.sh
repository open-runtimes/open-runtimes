#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/build-code

# Prepare Deno cache folder
mkdir -p /usr/build-code/deno-cache

# Set Deno Cache directory
export DENO_DIR="/usr/build-code/deno-cache"

# Cache Server Depdenencies
cd /usr/local/src/
deno cache server.ts

# Cache user function depdenencies
cd /usr/build-code
deno cache $ENTRYPOINT_NAME

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .