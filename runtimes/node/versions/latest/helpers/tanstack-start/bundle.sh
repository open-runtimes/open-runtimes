#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

IS_SSR=0

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Detect SSR in custom output directory
ENTRYPOINT="./server/index.mjs"
if [ -e "$ENTRYPOINT" ]; then
	IS_SSR=1 # NitroV2, NitroV3 (standalone)
fi

ENTRYPOINT="./server/server.js"
if [ -e "$ENTRYPOINT" ]; then
	IS_SSR=1 # Native SSR (middleware)
fi

# If SSR not detected yet, try default folders
if [ "$IS_SSR" -eq 0 ]; then
	cd /usr/local/build
	if [ -d "dist" ]; then
		cd ./dist
		ENTRYPOINT="./server/server.js"
		if [ -e "$ENTRYPOINT" ]; then
			IS_SSR=1 # Native SSR (middleware)
			OPEN_RUNTIMES_OUTPUT_DIRECTORY="./dist"
		fi
	fi

	if [ -d ".output" ]; then
		cd ./.output
		ENTRYPOINT="./server/index.mjs"
		if [ -e "$ENTRYPOINT" ]; then
			IS_SSR=1 # NitroV2, NitroV3 (standalone)
			OPEN_RUNTIMES_OUTPUT_DIRECTORY="./.output"
		fi
	fi
fi

# Change back to output directory before bundling
cd /usr/local/build
if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

if [ "$IS_SSR" -eq 1 ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/node_modules/ ./node_modules/
	rm -rf ./server/node_modules

	if [[ "${DISABLE_MODCLEAN,,}" != "true" ]]; then
		modclean --patterns default:safe --no-progress --run
	fi

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
