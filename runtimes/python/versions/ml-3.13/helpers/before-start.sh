#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for start ..."

# Extract code (handles sidecar pre-extraction)
. /usr/local/server/helpers/extract-code.sh

# Enter server folder
cd /usr/local/server

. /usr/local/server/helpers/prepare-start.sh

echo 'Starting ...'
