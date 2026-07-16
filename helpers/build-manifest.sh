#!/bin/bash
# Writes a JSON build manifest describing the build output (the current
# working directory) to $OPEN_RUNTIMES_BUILD_MANIFEST, for orchestrators that
# inspect the output post-job (e.g. rendering detection over the file
# listing). Opt-in: a no-op when the variable is unset. Best-effort: never
# fails the build.
#
# Manifest shape (v1) — future fields become sibling keys:
#   {"version":1,"files":["index.html","server/entry.mjs"]}

log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m $1 \e[0m"
}

if [ -z "${OPEN_RUNTIMES_BUILD_MANIFEST:-}" ]; then
	exit 0
fi

manifest_dir="${OPEN_RUNTIMES_BUILD_MANIFEST%/*}"
if [ ! -d "$manifest_dir" ] && ! mkdir -p "$manifest_dir"; then
	log 'Build manifest skipped: manifest directory could not be created.'
	exit 0
fi

tmp_manifest="${OPEN_RUNTIMES_BUILD_MANIFEST}.tmp"
rm -f "$tmp_manifest"
trap 'rm -f "$tmp_manifest"' EXIT

# The output file listing: dependency directories pruned, archive artifacts
# excluded (mirroring the archive step's excludes), leading ./ stripped, and
# capped so a pathological output can't produce an unbounded manifest.
max_files="${OPEN_RUNTIMES_BUILD_MANIFEST_MAX_FILES:-10000}"
files=$(find . -name node_modules -prune -o -type f ! -name code.sqfs ! -name code.tar ! -name code.tar.gz ! -name code.gz -print 2>/dev/null | head -n "$max_files" | sed 's|^\./||')

# jq guarantees correct JSON string escaping; the fallback covers the common
# cases (backslash, double quote) for images without jq.
if command -v jq >/dev/null 2>&1; then
	if ! printf '%s\n' "$files" | jq -R . | jq -s '{version: 1, files: [.[] | select(length > 0)]}' >"$tmp_manifest" 2>/dev/null; then
		log 'Build manifest warning: failed to encode manifest, continuing without it.'
		exit 0
	fi
else
	{
		printf '{"version":1,"files":['
		first=1
		while IFS= read -r file; do
			if [ -z "$file" ]; then
				continue
			fi
			escaped=$(printf '%s' "$file" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
			if [ "$first" -eq 1 ]; then
				first=0
			else
				printf ','
			fi
			printf '"%s"' "$escaped"
		done <<<"$files"
		printf ']}'
	} >"$tmp_manifest"
fi

if mv -f "$tmp_manifest" "$OPEN_RUNTIMES_BUILD_MANIFEST"; then
	count=$(printf '%s' "$files" | grep -c . || true)
	log "Build manifest written. ($count files)"
else
	log 'Build manifest warning: failed to write manifest, continuing without it.'
fi

exit 0
