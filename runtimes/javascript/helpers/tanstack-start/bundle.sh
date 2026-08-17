#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

IS_SSR=0
RESOLVED=0

cd /usr/local/build

# TanStack Start writes .output with a nitro plugin registered and dist without
# one, and the directory is configured before the build runs. When the build
# went to the other layout, take the same directory there instead of failing on
# one that was never written.
if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ] && [ -z "$(ls -A "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" 2>/dev/null)" ]; then
	CONFIGURED="${OPEN_RUNTIMES_OUTPUT_DIRECTORY#./}"
	CONFIGURED="${CONFIGURED%/}"

	case "$CONFIGURED" in
	.output) COUNTERPART="./dist" ;;
	dist) COUNTERPART="./.output" ;;
	.output/public) COUNTERPART="./dist/client" ;;
	dist/client) COUNTERPART="./.output/public" ;;
	*) COUNTERPART="" ;;
	esac

	if [ -n "$COUNTERPART" ] && [ -n "$(ls -A "$COUNTERPART" 2>/dev/null)" ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m Build wrote $COUNTERPART, not the configured $OPEN_RUNTIMES_OUTPUT_DIRECTORY. Using $COUNTERPART. \e[0m"
		OPEN_RUNTIMES_OUTPUT_DIRECTORY="$COUNTERPART"
		echo -n "$COUNTERPART" >/tmp/.opr-output-directory
		RESOLVED=1
	fi
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

# If SSR not detected yet, try default folders. This one stays local to the
# bundling below: a static build has no server entry either, and its configured
# directory is not wrong just because the server tree sits next to it.
if [ "$IS_SSR" -eq 0 ] && [ "$RESOLVED" -eq 0 ]; then
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
