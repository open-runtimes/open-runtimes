#!/bin/bash

helpers_dir="${BASH_SOURCE[0]%/*}"
source "$helpers_dir/build-cache-env.sh"

tmp_artifact="${OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT}.tmp"
rm -f "$tmp_artifact"
artifact_dir="${OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT%/*}"

if ! command -v mksquashfs >/dev/null 2>&1; then
	echo '[build cache] Warning: missing mksquashfs, continuing without cache save.'
	exit 0
fi

if [ ! -d "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" ]; then
	echo '[build cache] Save skipped: cache root missing.'
	exit 0
fi

if [ ! -d "$artifact_dir" ]; then
	echo '[build cache] Save skipped: artifact directory missing.'
	exit 0
fi

processors=$(nproc 2>/dev/null || echo 1)

if [ -n "${OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS:-}" ] && command -v timeout >/dev/null 2>&1; then
	mksquashfs_cmd=(timeout "$OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS" mksquashfs)
else
	mksquashfs_cmd=(mksquashfs)
fi

if "${mksquashfs_cmd[@]}" "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" "$tmp_artifact" -comp lz4 -b 1M -noappend -no-xattrs -no-progress -processors "$processors"; then
	if [ -n "${OPEN_RUNTIMES_BUILD_CACHE_MAX_SIZE_BYTES:-}" ]; then
		size=$(wc -c < "$tmp_artifact" | tr -d ' ')
		if [ "$size" -gt "$OPEN_RUNTIMES_BUILD_CACHE_MAX_SIZE_BYTES" ]; then
			rm -f "$tmp_artifact"
			echo '[build cache] Save skipped: artifact too large.'
			exit 0
		fi
	fi

	mv "$tmp_artifact" "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT"
	if [ -f "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" ]; then
		echo '[build cache] Saved.'
	fi
else
	rm -f "$tmp_artifact"
	echo '[build cache] Warning: failed to save cache, build result preserved.'
fi

exit 0
