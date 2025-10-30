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

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	mkdir -p /tmp/.opr-tmp
	mv "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" /tmp/.opr-tmp

	mkdir -p "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"

	mv /tmp/.opr-tmp/.next/standalone/* ./

	# Only copy public and static, rest is there
	if [ -d "/usr/local/build/public/" ]; then
		mkdir -p ./public
		mv /usr/local/build/public/* ./public/
	fi

	if [ -d "/tmp/.opr-tmp/.next/static/" ]; then
		mkdir -p ./.next/static
		mv /tmp/.opr-tmp/.next/static/* ./.next/static/
	fi

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"

elif [ -e "$WEBPACK_ENTRYPOINT" ] || [ -e "$TURBOPACK_ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	mkdir -p /tmp/.opr-tmp
	mv "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" /tmp/.opr-tmp

	mkdir -p "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	mv /tmp/.opr-tmp/* .next/

	# Copy over public folder, package.json, next config, and node_modules
	if [ -d "/usr/local/build/public/" ]; then
		mv /usr/local/build/public/ ./public/
	fi

	mv /usr/local/build/package*.json ./
	mv /usr/local/build/next.config.* ./
	mv /usr/local/build/node_modules/ ./node_modules/

	modclean --patterns default:safe --no-progress --run

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
