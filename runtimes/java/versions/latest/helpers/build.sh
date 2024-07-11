#!/bin/bash
# Fail build if any command fails
set -e

/bin/bash -c "source /usr/local/server/helpers/before-build.sh"
bash -c "$1"
/bin/bash -c "source /usr/local/server/helpers/after-build.sh"
