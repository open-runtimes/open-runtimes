#!/bin/sh
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-build.sh
conda run -n base /bin/bash -c "$1"
. /usr/local/server/helpers/after-build.sh