#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if ! command -v unsquashfs >/dev/null 2>&1 || ! command -v mksquashfs >/dev/null 2>&1; then
	echo '[build cache] Missing squashfs-tools in runtime image.'
	exit 1
fi

CACHE_ROOT=/usr/local/cache/build
mkdir -p "$CACHE_ROOT"

export npm_config_cache="$CACHE_ROOT/npm"
export YARN_CACHE_FOLDER="$CACHE_ROOT/yarn"
export npm_config_store_dir="$CACHE_ROOT/pnpm"
export XDG_CACHE_HOME="$CACHE_ROOT/xdg-cache"
export XDG_STATE_HOME="$CACHE_ROOT/xdg-state"
export BUN_INSTALL_CACHE_DIR="$CACHE_ROOT/bun"

if [ -f /cache/stores.sqfs ]; then
	echo '[build cache] Hit.'
	rm -rf "$CACHE_ROOT"
	mkdir -p "$CACHE_ROOT"
	unsquashfs -q -f -d "$CACHE_ROOT" /cache/stores.sqfs
else
	echo '[build cache] Miss.'
fi

set +e
bash /cache/build-command.sh
status=$?
set -e

if [ "$status" -eq 0 ] && [ -d "$CACHE_ROOT" ]; then
	processors=$(nproc 2>/dev/null || echo 1)
	if mksquashfs "$CACHE_ROOT" /cache/stores.sqfs -comp lz4 -b 1M -noappend -no-xattrs -no-progress -processors "$processors"; then
		echo '[build cache] Saved.'
	else
		echo '[build cache] Warning: failed to save cache, build result preserved.'
	fi
fi

exit "$status"
