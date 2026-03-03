#!/bin/bash
set -e
shopt -s dotglob

cd ./runtimes/.test

# Detect multi-stage Dockerfiles with build/runtime targets
if grep -q "AS build" Dockerfile; then
	docker build --platform linux/x86_64 --target build -t open-runtimes/test-runtime-build .
	docker build --platform linux/x86_64 --target runtime -t open-runtimes/test-runtime .
else
	docker build --platform linux/x86_64 -t open-runtimes/test-runtime .
fi

cd ../../
