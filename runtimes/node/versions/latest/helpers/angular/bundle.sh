#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

ENTRYPOINT="./server/server.mjs"
if [ -e "$ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/node_modules/ ./node_modules/

	modclean --patterns default:safe --no-progress --run

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
