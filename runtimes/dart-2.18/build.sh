#!/bin/sh

# Fail build if any command fails
set -e

# Copy User Code
cp -R /usr/code/* /usr/local/src/user_code

# Rename Main Function Dart
mkdir -p /usr/local/src/user_code/lib
mv -n /usr/code/$INTERNAL_RUNTIME_ENTRYPOINT /usr/local/src/user_code/lib/main.dart

cd /usr/local/src/user_code/

# Add a pubspec.yaml if one doesn't already exist.
if [ ! -f "pubspec.yaml" ]; then
    cp /usr/local/src/pubspec.yaml.fallback /usr/local/src/user_code/pubspec.yaml
fi

INSTALL_COMMAND=${1:-'dart pub get'}
BUILD_COMMAND=${2:-'dart compile exe server.dart -o runtime'}

# Move to prepare_package script directory
cd /usr/local/src/prepare
dart pub get
dart prepare.dart

# Move to server directory
cd /usr/local/src
dart pub get

cd /usr/local/src/user_code
# Get user code dependencies

eval "$INSTALL_COMMAND"

# Move back to server directory
cd /usr/local/src

# Compile the Code
eval "$BUILD_COMMAND"

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz runtime