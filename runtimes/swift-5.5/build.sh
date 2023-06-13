#!/bin/sh

# Fail build if any command fails
set -e

cp -a /usr/code/. /usr/local/src/Sources/Runtime
cd /usr/local/src
if [ -f "./Sources/Runtime/Package.swift" ]; then
    cd ./Sources/Runtime
    swift ../../dependencies.swift
    rm "Package.swift"
    cd /usr/local/src
fi
rm "dependencies.swift"
swift package resolve
swift build --configuration release
cp "$(swift build --configuration release --show-bin-path)/Runtime" /usr/workspace/
tar -zcf /usr/code/code.tar.gz -C /usr/workspace .