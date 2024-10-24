#!/bin/sh
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-start.sh

echo "Start static web server..."

static-web-server -p 3000 --log-level debug --compression false -d /usr/local/server/src/function/dist