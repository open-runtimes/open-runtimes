#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

IS_SSR=0

cd /usr/local/build

# TanStack Start writes .output with a nitro plugin registered and dist without
# one, and the directory is configured before the build runs. A site created
# before its layout was known carries no value at all, and an empty one is not a
# choice to override: resolve it below from what the build wrote, the way the
# rendering strategy is left for the first build to decide. A directory the user
# did set is obeyed even when the build wrote elsewhere.
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

# If SSR not detected yet, try default folders. What this finds is handed back
# only when nothing was configured; against a directory the user set it stays
# local to the bundling below, since a static site's directory is not wrong just
# because the server tree sits next to it.
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

# No server anywhere means the build was prerendered, and its assets sit one
# level into whichever layout it chose.
if [ "$DETECT" -eq 1 ] && [ -z "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	for CANDIDATE in ./.output/public ./dist/client; do
		if [ -d "$CANDIDATE" ]; then
			OPEN_RUNTIMES_OUTPUT_DIRECTORY="$CANDIDATE"
			break
		fi
	done
fi

# This script is appended to the build command, so it runs in that subshell and
# a directory resolved here cannot reach the pack and archive phases through the
# environment. Leave it in the file the lifecycle reads instead.
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
