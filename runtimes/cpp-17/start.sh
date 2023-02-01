#!/usr/bin/env sh

cd /usr/code
tar -zxf /tmp/code.tar.gz

set -o allexport
source ./.open-runtimes
set +o allexport

./cpp_runtime