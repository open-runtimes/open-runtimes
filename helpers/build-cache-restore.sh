#!/bin/bash

helpers_dir="${BASH_SOURCE[0]%/*}"
source "$helpers_dir/build-cache-env.sh"

mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"

if ! command -v unsquashfs >/dev/null 2>&1; then
	echo '[build cache] Warning: missing unsquashfs, continuing without cache restore.'
	exit 0
fi

if [ ! -f "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" ]; then
	echo '[build cache] Miss.'
	exit 0
fi

rm -rf "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"

if unsquashfs -q -f -d "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT"; then
	echo '[build cache] Hit.'
else
	echo '[build cache] Warning: failed to restore cache, starting fresh.'
	rm -rf "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
	mkdir -p "$OPEN_RUNTIMES_BUILD_CACHE_ROOT"
fi

exit 0
