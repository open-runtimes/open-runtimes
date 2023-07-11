#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Copy from mounted volume to temporary folder
cp -R /mnt/code/* /usr/local/build

# Add a pubspec.yaml if one doesn't already exist.
cd /usr/local/build
if [ ! -f "pubspec.yaml" ]; then
    cp /usr/local/server/pubspec.yaml.fallback /usr/local/build/pubspec.yaml
fi

# Run prepare_package script
cd /usr/local/server/prepare
dart pub get
dart prepare.dart

# Get server dependencies
cd /usr/local/server
dart pub get

# Enter build folder
cd /usr/local/build

echo 'Building ...'