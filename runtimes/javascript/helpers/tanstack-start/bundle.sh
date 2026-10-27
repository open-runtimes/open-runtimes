#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

IS_SSR=0

cd /usr/local/build

# TanStack Start writes .output with a nitro plugin registered, dist without one,
# and the directory is configured before the build runs. Nothing configured means
# nothing to override, so let the build decide.
DETECT=0
if [ -z "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	DETECT=1
fi

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

	# Back to the build root, so .output is looked for beside dist and not inside it
	cd /usr/local/build
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

# A prerendered build has no server; its assets sit one level in
if [ "$DETECT" -eq 1 ] && [ -z "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	for CANDIDATE in ./.output/public ./dist/client; do
		if [ -d "$CANDIDATE" ]; then
			OPEN_RUNTIMES_OUTPUT_DIRECTORY="$CANDIDATE"
			break
		fi
	done
fi

# Hand it back to the lifecycle, which cannot see a variable set in this subshell
if [ "$DETECT" -eq 1 ] && [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Build output resolved to $OPEN_RUNTIMES_OUTPUT_DIRECTORY. \e[0m"
	echo -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" >/tmp/.opr-output-directory
fi

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

if [ "$IS_SSR" -eq 1 ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/node_modules/ ./node_modules/
	rm -rf ./server/node_modules

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
