#!/bin/bash
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-build.sh
bash -c "$1"
. /usr/local/server/helpers/after-build.sh
