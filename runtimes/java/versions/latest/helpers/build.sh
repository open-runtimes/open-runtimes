#!/bin/bash
# Fail build if any command fails
set -e

bash -c "source /usr/local/server/helpers/before-build.sh"
bash -c "$1"
bash -c "source /usr/local/server/helpers/after-build.sh"
