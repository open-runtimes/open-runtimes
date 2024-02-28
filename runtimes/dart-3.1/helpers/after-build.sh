#!/bin/sh
# Fail build if any command fails
set -e

# Get server dependencies
cd /usr/local/server
dart pub get

echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server
dart compile exe src/server.dart -o server
mv /usr/local/server/server /usr/local/build/compiled/server

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."