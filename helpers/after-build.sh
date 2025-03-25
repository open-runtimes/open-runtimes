#!/bin/sh
# Fail build if any command fails
set -e

# Install dependencies
cd /usr/local/server

. /usr/local/server/helpers/prepare-compile.sh

echo "Packing build ..."

. /usr/local/server/helpers/prepare-packing.sh

# Finish build by preparing tar to use for starting the runtime
cd /usr/local/build/
if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"  ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

# Check if the output directory is empty
if [ "$(ls -A .)" ]; then
    # Store entrypoint into build. Will be used during start process
    touch .open-runtimes
    echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > .open-runtimes

    tar --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

    echo "Build finished."
else
    echo "Error: Build output directory is empty. Ensure your output directory is configured correctly."
    exit 1
fi