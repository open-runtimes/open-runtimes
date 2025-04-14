#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Get server dependencies
dart pub get

echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server
dart compile exe src/server.dart -o server
mv /usr/local/server/server /usr/local/build/compiled/server
