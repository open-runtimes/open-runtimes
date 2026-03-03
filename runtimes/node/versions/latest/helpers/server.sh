#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

node --max_old_space_size=${OPEN_RUNTIMES_MAX_OLD_SPACE_SIZE:-8192} src/server.js
