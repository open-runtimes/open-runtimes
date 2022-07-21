#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds


# Cache server and user function dependencies
cd /usr/builds
deno vendor /usr/local/src/server.ts $INTERNAL_RUNTIME_ENTRYPOINT
mkdir -p vendor

# Check for build error
deno check $INTERNAL_RUNTIME_ENTRYPOINT

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .