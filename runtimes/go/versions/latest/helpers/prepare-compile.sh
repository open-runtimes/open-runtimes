#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Compiling ..."

# Compile the Code
cd /usr/local/server/src
go get openruntimes/handler@v0.0.0
go build -ldflags="-s -w"
mv /usr/local/server/src/server /usr/local/build/server
