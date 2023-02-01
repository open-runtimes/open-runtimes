#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Install User Function Dependencies if package.json exists
cd /usr/builds
if [ -f package.json ]; then
  npm install
fi

mkdir -p node_modules

# Merge the node_modules from the server into the user's node_modules to be restored later.
cp -R /usr/local/src/node_modules/* /usr/builds/node_modules

touch .open-runtimes
echo "INTERNAL_RUNTIME_ENTRYPOINT=$INTERNAL_RUNTIME_ENTRYPOINT" > .open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .