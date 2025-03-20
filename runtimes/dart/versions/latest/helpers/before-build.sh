#!/bin/sh
# Fail build if any command fails
set -e

echo "Preparing for build ..."

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
    echo "ERROR: No source code found. Ensure your source directory exists and isn't empty."
    exit 1
fi

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

# Enter build folder
cd /usr/local/build

echo 'Building ...'
