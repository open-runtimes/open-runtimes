#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

WEBPACK_ENTRYPOINT="./server/webpack-runtime.js"
TURBOPACK_ENTRYPOINT="./turbopack"
STANDALONE_ENTRYPOINT="./standalone/server.js"

if [ -e "$STANDALONE_ENTRYPOINT" ]; then
    echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Detected standalone Next.js build. \e[0m"
    
    # Replace empty with .
    OPEN_RUNTIMES_OUTPUT_DIRECTORY="${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-.}"
    # Add /standalone, while removing right-trailing slashes
    OPEN_RUNTIMES_OUTPUT_DIRECTORY="${OPEN_RUNTIMES_OUTPUT_DIRECTORY%/}/standalone"
fi

if [ -e "$WEBPACK_ENTRYPOINT" ] || [ -e "$TURBOPACK_ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	mkdir -p /tmp/.opr-tmp
	mv "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" /tmp/.opr-tmp

	mkdir -p "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	mv /tmp/.opr-tmp/* .next/

	if [ -d "/usr/local/build/public/" ]; then
		mv /usr/local/build/public/ ./public/
	fi

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/next.config.* ./
	mv /usr/local/build/node_modules/ ./node_modules/

	modclean --patterns default:safe --no-progress --run

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
