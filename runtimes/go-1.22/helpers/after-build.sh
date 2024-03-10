#!/bin/sh
# Fail build if any command fails
set -e

echo "Compiling ..."

# Compile the Code
cd /usr/local/server/src
go get openruntimes/handler@v0.0.0
go build
mv /usr/local/server/src/server /usr/local/build/server

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."