#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for build ..."

# Check if source directory exists and has files
if [ ! -d "/mnt/code" ] || [ -z "$(ls -A /mnt/code 2>/dev/null)" ]; then
	echo -e "\e[90m$(date '+%H:%M:%S') \e[31m[\e[0mopen-runtimes\e[31m]\e[31m Error: No source code found. Ensure your source isn't empty. \e[0m"
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
