#!/bin/sh
# Fail build if any command fails
set -e

# Install dependencies
cd /usr/local/server

. /usr/local/server/helpers/prepare-compile.sh

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/.open-runtimes

. /usr/local/server/helpers/prepare-packing.sh

# Finish build by preparing tar to use for starting the runtime
cd /usr/local/build
tar -C /usr/local/build --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."