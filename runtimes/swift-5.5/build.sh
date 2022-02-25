#!/bin/sh

# Fail build if any command fails
set -e

cp -a /usr/code/. /usr/local/src/Sources/App/Custom/
cd /usr/local/src
if [ -f "./Sources/App/Custom/Package.swift" ]; then
    cd ./Sources/App/Custom
    swift ../dependencies.swift
    rm "Package.swift" 
    cd /usr/local/src
fi
rm "Sources/App/dependencies.swift"
swift package resolve
swift build --configuration release
cp "$(swift build --configuration release --show-bin-path)/Run" /usr/workspace/
tar -zcf /usr/code/code.tar.gz -C /usr/workspace .