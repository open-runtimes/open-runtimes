#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepare empty folder to prevent errors with copying
mkdir -p /usr/local/build/vendor

