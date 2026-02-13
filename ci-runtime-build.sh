#!/bin/bash
set -e
shopt -s dotglob

cd ./runtimes/.test
docker build --platform linux/x86_64 -t open-runtimes/test-runtime .
cd ../../
