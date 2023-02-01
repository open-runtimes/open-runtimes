#!/bin/sh

# Fail build if any command fails
set -e

mkdir -p /usr/builds
cp -a /usr/code/. /usr/builds/

if [[ ! -f "usr/builds/Package.swift" ]]; then
    mv /usr/local/src/Package.swift.fallback /usr/builds/Package.swift
fi

cp -a /usr/builds/. /usr/local/src/Sources/App/Custom/
cd /usr/local/src

cd ./Sources/App/Custom
swift ../dependencies.swift
rm "Package.swift" 
cd /usr/local/src

rm "Sources/App/dependencies.swift"

INSTALL_COMMAND=${1:-'swift package resolve'}
BUILD_COMMAND=${2:-'swift build --configuration release'}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

cp "$(swift build --configuration release --show-bin-path)/Run" /usr/workspace/
tar -zcf /usr/code/code.tar.gz -C /usr/workspace .