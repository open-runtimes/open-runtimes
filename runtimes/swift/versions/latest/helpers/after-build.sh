#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

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

echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
swift package resolve
swift build --configuration release --static-swift-stdlib
cp "$(swift build --configuration release --show-bin-path)/Runtime" /usr/local/build/compiled/

# Strip debug symbols to reduce binary size
strip /usr/local/build/compiled/Runtime

# Clean up build artifacts to reduce image size
rm -rf /usr/local/server/.build

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" >/usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
if [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "none" ]; then
	tar -C /usr/local/build/compiled --exclude code.tar.gz -cf /mnt/code/code.tar.gz .
else
	# Default to gzip
	tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .
fi

echo "Build finished."
