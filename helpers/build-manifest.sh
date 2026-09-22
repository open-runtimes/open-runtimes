#!/bin/bash
# Writes a JSON build manifest describing the build output (the current
# working directory) to $OPEN_RUNTIMES_BUILD_MANIFEST, for orchestrators that
# inspect the output post-job (e.g. rendering detection over the file
# listing). Opt-in: a no-op when the variable is unset. Best-effort: never
# fails the build.
#
# The listing is a whitelist, not a walk: the SSR framework entrypoints below
# that exist, plus the first two HTML files in the output. Outputs can hold
# millions of generated files, so everything detection does not look for by
# name stays out.
#
# Manifest shape (v1) — future fields become sibling keys:
#   {"version":1,"files":["index.html","server/entry.mjs"]}

log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m $1 \e[0m"
}

if [ -z "${OPEN_RUNTIMES_BUILD_MANIFEST:-}" ]; then
	exit 0
fi

manifest_dir=$(dirname "$OPEN_RUNTIMES_BUILD_MANIFEST")
if [ ! -d "$manifest_dir" ] && ! mkdir -p "$manifest_dir"; then
	log 'Build manifest skipped: manifest directory could not be created.'
	exit 0
fi

tmp_manifest="${OPEN_RUNTIMES_BUILD_MANIFEST}.tmp"
rm -f "$tmp_manifest"
trap 'rm -f "$tmp_manifest"' EXIT

# SSR entrypoints, mirroring the detector's framework file list. Add a path
# here when a new SSR framework is supported.
candidates=(
	'.next/server/webpack-runtime.js' # nextjs
	'.next/turbopack'                 # nextjs
	'server.js'                       # nextjs
	'server/index.mjs'                # nuxt, analog, tanstack-start
	'handler.js'                      # sveltekit
	'server/entry.mjs'                # astro
	'build/server/index.js'           # remix
	'server/server.mjs'               # angular
	'server/server.js'                # tanstack-start
)

# The candidates that exist (-e, since some markers are directories), then the
# first two HTML files.
files=$(
	for candidate in "${candidates[@]}"; do
		if [ -e "$candidate" ]; then
			printf '%s\n' "$candidate"
		fi
	done
	find . -name node_modules -prune -o -type f -name '*.html' -print 2>/dev/null | sed 's|^\./||' | head -n 2
)

# jq guarantees correct JSON string escaping; the fallback covers the common
# cases (backslash, double quote, tab, carriage return) for images without
# jq — remaining control characters are rare enough in file names that the
# consumer's tolerance for an unparseable manifest is the backstop.
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
			escaped=$(printf '%s' "$file" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e $'s/\t/\\\\t/g' -e $'s/\r/\\\\r/g')
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
