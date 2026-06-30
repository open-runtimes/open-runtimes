#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepare Deno cache folder
mkdir -p /usr/local/build/deno-cache

# Set Deno Cache directory
export DENO_DIR="/usr/local/build/deno-cache"
