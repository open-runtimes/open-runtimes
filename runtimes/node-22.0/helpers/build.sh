#!/bin/sh
# Fail build if any command fails
set -e

source /usr/local/server/helpers/before-build.sh
sh -c "$1"
source /usr/local/server/helpers/after-build.sh