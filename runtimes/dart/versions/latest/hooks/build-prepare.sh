#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Add a pubspec.yaml if one doesn't already exist.
if [ ! -f "pubspec.yaml" ]; then
	cp /usr/local/server/pubspec.yaml.fallback /usr/local/build/pubspec.yaml
fi

# Run prepare_package script
cd /usr/local/server/prepare
dart pub get
dart prepare.dart
