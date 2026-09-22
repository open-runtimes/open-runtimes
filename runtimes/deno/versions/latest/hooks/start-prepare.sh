#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export DENO_DIR="/usr/local/server/src/function/deno-cache"
