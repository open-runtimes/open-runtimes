#!/bin/bash
# Fail build if any command fails
set -e

source /usr/local/server/helpers/before-start.sh
sh -c "$1"