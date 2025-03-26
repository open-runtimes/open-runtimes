#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Install dependencies
cd /usr/local/server

. /usr/local/server/helpers/prepare-compile.sh

echo "[37mPacking build .. [0m"

. /usr/local/server/helpers/prepare-packing.sh

# Finish build by preparing tar to use for starting the runtime
cd /usr/local/build/
if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"  ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

# Store entrypoint into build. Will be used during start process
touch .open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > .open-runtimes

tar --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "[37mBuild finished. [0m"