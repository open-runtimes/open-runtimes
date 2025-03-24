#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/before-start.sh
bash -c "$1"