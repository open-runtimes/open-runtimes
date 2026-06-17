#!/bin/bash

helpers_dir="${BASH_SOURCE[0]%/*}"
source "$helpers_dir/build-cache-env.sh"

log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m $1 \e[0m"
}

mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"

if ! command -v unsquashfs >/dev/null 2>&1; then
	log 'Build cache warning: missing unsquashfs, continuing without cache restore.'
	exit 0
fi

if [ ! -f "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" ]; then
	log 'Build cache miss.'
	exit 0
fi

rm -rf "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"

if unsquashfs -q -f -d "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" >/dev/null 2>&1; then
	log 'Build cache hit.'
else
	log 'Build cache warning: failed to restore cache, starting fresh.'
	rm -rf "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
	mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
fi

exit 0
