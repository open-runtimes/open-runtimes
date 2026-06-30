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
swift build --configuration release
cp "$(swift build --configuration release --show-bin-path)/Runtime" /usr/local/build/compiled/
