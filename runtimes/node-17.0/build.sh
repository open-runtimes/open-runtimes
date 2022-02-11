#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/build-code

# Install User Function Dependencies if package.json exists
cd /usr/build-code
if [ -f package.json ]; then
  npm install
fi

mkdir -p node_modules

# Merge the node_modules from the server into the user's node_modules to be restored later.
cp -R /usr/local/src/node_modules/* /usr/build-code/node_modules

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .