#!/bin/sh
echo "Packing build ..."

# Prepare empty node_modules to prevent errors with copying
mkdir -p /usr/builds/node_modules

# Merge the node_modules from the server into the user's node_modules
cp -R /usr/local/src/node_modules/* /usr/builds/node_modules

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/builds --exclude code.tar.gz -zcf /usr/code/code.tar.gz .

echo "Build finished."