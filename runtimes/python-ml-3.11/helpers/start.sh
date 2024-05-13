#!/bin/sh
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-start.sh
conda run -n base "$1"