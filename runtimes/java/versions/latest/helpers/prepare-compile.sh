#!/bin/bash
echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
sh gradlew buildJar

# Copy output files
cp -R /usr/local/server/build/libs/* /usr/local/build/compiled
