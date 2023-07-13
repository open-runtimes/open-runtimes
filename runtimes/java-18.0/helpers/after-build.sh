#!/bin/sh
# Fail build if any command fails
set -e

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Copy output files
cp -R /usr/local/server/build/libs/* /usr/local/build/compiled

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."