#!/bin/sh
# Fail build if any command fails
set -e

# Prepare dependencies
cp -a /usr/local/build/. /usr/local/server/Sources/Runtime
cd /usr/local/server
if [ -f "./Sources/Runtime/Package.swift" ]; then
    cd ./Sources/Runtime
    swift ../../dependencies.swift
    rm "Package.swift"
    cd /usr/local/server
fi
rm "dependencies.swift"

echo "Compiling"

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
swift package resolve
swift build --configuration release
cp "$(swift build --configuration release --show-bin-path)/Runtime" /usr/local/build/compiled/

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."