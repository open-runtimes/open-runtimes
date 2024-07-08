#!/bin/bash
# Fail build if any command fails
set -e

/bin/bash -c "source /usr/local/server/helpers/before-start.sh"
bash -c "$1"
