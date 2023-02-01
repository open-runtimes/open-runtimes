#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace 
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz

set -o allexport
source /usr/code-start/.open-runtimes
set +o allexport

export DENO_DIR="/usr/code-start/deno-cache"
cd /usr/local/src/
denon run --allow-net --allow-write --allow-read --allow-env server.ts
