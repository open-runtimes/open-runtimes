#!/bin/bash
# nft + prune node_modules to reduce build output size

# Skip if NFT is disabled
if [[ "${OPEN_RUNTIMES_NFT:-}" == "disabled" ]]; then
	return 0 2>/dev/null || exit 0
fi

# Determine output directory
OUTPUT_DIR="/usr/local/build"
if [ -n "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-}" ]; then
	OUTPUT_DIR="/usr/local/build/$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Skip if no node_modules (not an SSR build)
if [ ! -d "$OUTPUT_DIR/node_modules" ]; then
	return 0 2>/dev/null || exit 0
fi

cd "$OUTPUT_DIR"

# Detect entrypoint from custom start command or framework output structure
ENTRYPOINT=""

if [ -n "${OPEN_RUNTIMES_START_COMMAND:-}" ]; then
	if [[ "$OPEN_RUNTIMES_START_COMMAND" =~ node[[:space:]]+([^[:space:]]+\.m?js) ]]; then
		ENTRYPOINT="${BASH_REMATCH[1]}"
	fi
elif [ -e "./server.js" ] && [ -d "./.next" ]; then
	# Next.js standalone
	ENTRYPOINT="./server.js"
elif [ -d "./.next" ]; then
	# Next.js non-standalone — trace the next package itself
	echo 'import "next";' > ./.nft-entry.mjs
	ENTRYPOINT="./.nft-entry.mjs"
elif [ -e "./server/entry.mjs" ]; then
	# Astro
	ENTRYPOINT="./server/entry.mjs"
elif [ -e "./server/server.mjs" ]; then
	# Angular
	ENTRYPOINT="./server/server.mjs"
elif [ -e "./handler.js" ]; then
	# SvelteKit
	ENTRYPOINT="./handler.js"
elif [ -e "./build/server/index.js" ]; then
	# Remix
	ENTRYPOINT="./build/server/index.js"
elif [ -e "./server/server.js" ]; then
	# TanStack Start native SSR
	ENTRYPOINT="./server/server.js"
elif [ -e "./server/index.mjs" ]; then
	# Nuxt / Analog / TanStack Start Nitro
	ENTRYPOINT="./server/index.mjs"
fi

if [ -z "$ENTRYPOINT" ]; then
	return 0 2>/dev/null || exit 0
fi

SIZE_BEFORE=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Tracing from $ENTRYPOINT (node_modules: ${SIZE_BEFORE}MB) \e[0m"

# Run NFT — capture exit code without triggering set -e
NFT_EXIT=0
NODE_PATH=/usr/local/server/node_modules node /usr/local/server/src/nft-trace.mjs "$ENTRYPOINT" "$OUTPUT_DIR" || NFT_EXIT=$?

NFT_MANIFEST="$OUTPUT_DIR/.nft-files"

if [ "$NFT_EXIT" -ne 0 ] || [ ! -f "$NFT_MANIFEST" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m NFT failed (exit $NFT_EXIT), skipping pruning. \e[0m"
	rm -f "$OUTPUT_DIR/.nft-entry.mjs"
	return 0 2>/dev/null || exit 0
fi

# Read null-delimited manifest, filter to node_modules/ entries only
mapfile -d '' ALL_FILES < "$NFT_MANIFEST"

NEEDED_FILES=()
for file in "${ALL_FILES[@]}"; do
	if [[ "$file" == node_modules/* ]]; then
		NEEDED_FILES+=("$file")
	fi
done

if [ "${#NEEDED_FILES[@]}" -eq 0 ]; then
	rm -f "$NFT_MANIFEST" "$OUTPUT_DIR/.nft-entry.mjs"
	return 0 2>/dev/null || exit 0
fi

# Delete everything in node_modules not referenced by the trace
(cd "$OUTPUT_DIR" && find node_modules -type f -o -type l) | sort > /tmp/.nft-all
printf '%s\n' "${NEEDED_FILES[@]}" | sort > /tmp/.nft-keep
comm -23 /tmp/.nft-all /tmp/.nft-keep | tr '\n' '\0' | xargs -0 rm -f
find "$OUTPUT_DIR/node_modules" -type d -empty -delete

SIZE_AFTER=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)
echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Pruned node_modules: ${SIZE_BEFORE}MB → ${SIZE_AFTER}MB \e[0m"

# Clean up
rm -f /tmp/.nft-all /tmp/.nft-keep "$NFT_MANIFEST" "$OUTPUT_DIR/.nft-entry.mjs"
