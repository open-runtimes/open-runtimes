#!/bin/bash

helpers_dir="${BASH_SOURCE[0]%/*}"
source "$helpers_dir/build-cache-env.sh"

log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m $1 \e[0m"
}

tmp_artifact="${OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT}.tmp"
rm -f "$tmp_artifact"
artifact_dir=$(dirname "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT")

# stat, not wc -c: busybox wc reads the whole artifact to count it
file_size() {
	stat -c %s "$1"
}

log_file_output() {
	local file="$1"

	if [ ! -s "$file" ]; then
		return
	fi

	while IFS= read -r line || [ -n "$line" ]; do
		log "Build cache mksquashfs output: $line"
	done <"$file"
}

if ! command -v mksquashfs >/dev/null 2>&1; then
	log 'Build cache warning: missing mksquashfs, continuing without cache save.'
	exit 0
fi

if [ ! -d "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" ]; then
	log 'Build cache save skipped: cache root missing.'
	exit 0
fi

if [ ! -d "$artifact_dir" ] && ! mkdir -p "$artifact_dir"; then
	log 'Build cache save skipped: artifact directory could not be created.'
	exit 0
fi

processors=$(nproc 2>/dev/null || echo 1)
tmp_log=$(mktemp "${artifact_dir}/build-cache-save.XXXXXX" 2>/dev/null || mktemp 2>/dev/null || true)

cleanup() {
	rm -f "$tmp_artifact"
	if [ -n "$tmp_log" ]; then
		rm -f "$tmp_log"
	fi
}

trap cleanup EXIT

old_size=''

if [ -f "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" ]; then
	old_size=$(file_size "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT")
fi

if [ -n "${OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS:-}" ] && command -v timeout >/dev/null 2>&1; then
	mksquashfs_cmd=(timeout "$OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS" mksquashfs)
else
	mksquashfs_cmd=(mksquashfs)
fi

if [ -z "$tmp_log" ]; then
	log 'Build cache warning: failed to create cache save log, build result preserved.'
	exit 0
fi

if "${mksquashfs_cmd[@]}" "$OPEN_RUNTIMES_BUILD_CACHE_ROOT" "$tmp_artifact" -comp lz4 -b 1M -noappend -no-xattrs -no-progress -processors "$processors" >"$tmp_log" 2>&1; then
	if [ -n "${OPEN_RUNTIMES_BUILD_CACHE_MAX_SIZE_BYTES:-}" ]; then
		size=$(file_size "$tmp_artifact")
		if [ "$size" -gt "$OPEN_RUNTIMES_BUILD_CACHE_MAX_SIZE_BYTES" ]; then
			log "Build cache save skipped: artifact too large ($size bytes, max $OPEN_RUNTIMES_BUILD_CACHE_MAX_SIZE_BYTES bytes)."
			exit 0
		fi
	fi

	if mv -f "$tmp_artifact" "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT"; then
		if [ ! -f "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT" ]; then
			log 'Build cache warning: failed to verify saved cache artifact, build result preserved.'
			exit 0
		fi

		new_size=$(file_size "$OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT")

		if [ -n "$old_size" ]; then
			log "Build cache saved. ($old_size bytes -> $new_size bytes)"
		else
			log "Build cache saved. ($new_size bytes)"
		fi
	else
		log 'Build cache warning: failed to save cache, build result preserved.'
	fi
else
	status=$?
	if [ -n "${OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS:-}" ] && [ "$status" -eq 124 ]; then
		log "Build cache warning: failed to save cache before timeout (${OPEN_RUNTIMES_BUILD_CACHE_SAVE_TIMEOUT_SECONDS}s), build result preserved."
	else
		log "Build cache warning: failed to save cache with exit code $status, build result preserved."
	fi
	log_file_output "$tmp_log"
	log 'Build cache warning: failed to save cache, build result preserved.'
fi

exit 0
