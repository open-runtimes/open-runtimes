#!/bin/bash
set -e
shopt -s dotglob

mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

# Runtime base dockerfile
cp -R "./runtimes/$RUNTIME_FOLDER/$RUNTIME_FOLDER.dockerfile" ./runtimes/.test

# Runtime-specific files (most)
cp -R "./runtimes/$RUNTIME_FOLDER/versions/latest"/* ./runtimes/.test

# Version-specific files
cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION_FOLDER"/* ./runtimes/.test

# Global Docker configuration
cp ./base-before.dockerfile ./runtimes/.test/base-before.dockerfile
cp ./base-after.dockerfile ./runtimes/.test/base-after.dockerfile
