#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server

sh gradlew buildJar

# Copy output files
cp -R /usr/local/server/build/libs/ /usr/local/build/compiled
