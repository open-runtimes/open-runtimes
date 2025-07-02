#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/webpack-runtime.js"
if [ -e "$ENTRYPOINT" ]; then
	echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR started. [0m"

	cd /usr/local/build

	mkdir -p /tmp/.opr-tmp
	mv $OPEN_RUNTIMES_OUTPUT_DIRECTORY /tmp/.opr-tmp

	mkdir -p $OPEN_RUNTIMES_OUTPUT_DIRECTORY
	cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
	mv /tmp/.opr-tmp/* .next/

	if [ -d "/usr/local/build/public/" ]; then
		mv /usr/local/build/public/ ./public/
	fi

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/node_modules/ ./node_modules/

	echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR finished. [0m"
fi
