#!/bin/sh
cd /usr/code
tar -zxf /tmp/code.tar.gz
rm /tmp/code.tar.gz

set -o allexport
source ./.open-runtimes
set +o allexport


./Run serve --env production --hostname 0.0.0.0 --port 3000