#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/before-start.sh
. /usr/local/server/helpers/timings.sh start listen
bash -c "$1"
