#!/bin/bash
# Prune node_modules to reduce build output size
# Strategies: NFT (opt-in tree-shaking) or modclean (default junk removal)

export OPEN_RUNTIMES_CLEANUP="none"

# Determine output directory
OUTPUT_DIR="/usr/local/build"
if [ -n "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-}" ]; then
	OUTPUT_DIR="/usr/local/build/$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Skip if no node_modules (not an SSR build)
if [ ! -d "$OUTPUT_DIR/node_modules" ]; then
	return 0 2>/dev/null || exit 0
fi

cd "$OUTPUT_DIR" || exit

# ─── NFT (opt-in) ────────────────────────────────────────────────────────────
if [[ "${OPEN_RUNTIMES_NFT:-}" == "enabled" ]]; then

	SIZE_BEFORE=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)

	# Detect entrypoint from custom start command or framework output structure
	ENTRYPOINT=""
	SSR_WRAPPER=""

	if [ -n "${OPEN_RUNTIMES_START_COMMAND:-}" ]; then
		if [[ "$OPEN_RUNTIMES_START_COMMAND" =~ node[[:space:]]+([^[:space:]]+\.m?js) ]]; then
			ENTRYPOINT="${BASH_REMATCH[1]}"
		fi
	elif [ -e "./server.js" ] && [ -d "./.next" ]; then
		# Next.js standalone
		ENTRYPOINT="./server.js"
	elif [ -d "./.next" ]; then
		# Next.js non-standalone — trace next + next.config + server-rendered pages/routes
		SSR_WRAPPER="/usr/local/server/src/server-next-js.mjs"
		{
			echo 'import "next";'
			# Trace next.config dependencies (e.g. @content-collections/next).
			# Can't import the config directly — NFT can't resolve .ts from .mjs.
			# Instead, extract its bare-specifier imports and add them individually.
			for cfg in ./next.config.ts ./next.config.mjs ./next.config.js; do
				if [ -e "$cfg" ]; then
					grep -oE "(from|require\s*\()\s*[\"']([^./][^\"']*)" "$cfg" \
						| grep -oE "[\"'][^\"']+" | tr -d "\"'" | while read -r dep; do
						echo "import \"$dep\";"
					done
					break
				fi
			done
			find .next/server/pages .next/server/app -name '*.js' -o -name '*.mjs' 2>/dev/null | while read -r f; do
				echo "import \"./$f\";"
			done
		} >./.nft-entry.mjs
		ENTRYPOINT="./.nft-entry.mjs"
	elif [ -e "./server/entry.mjs" ]; then
		# Astro
		ENTRYPOINT="./server/entry.mjs"
		SSR_WRAPPER="/usr/local/server/src/server-astro.mjs"
	elif [ -e "./server/server.mjs" ]; then
		# Angular
		ENTRYPOINT="./server/server.mjs"
		SSR_WRAPPER="/usr/local/server/src/server-angular.mjs"
	elif [ -e "./handler.js" ]; then
		# SvelteKit
		ENTRYPOINT="./handler.js"
		SSR_WRAPPER="/usr/local/server/src/server-sveltekit.mjs"
	elif [ -e "./build/server/index.js" ]; then
		# Remix
		ENTRYPOINT="./build/server/index.js"
		SSR_WRAPPER="/usr/local/server/src/server-remix.mjs"
	elif [ -e "./server/server.js" ]; then
		# TanStack Start native SSR
		ENTRYPOINT="./server/server.js"
		SSR_WRAPPER="/usr/local/server/src/server-tanstack-start.mjs"
	elif [ -e "./server/index.mjs" ]; then
		# Nuxt / Analog / TanStack Start Nitro
		ENTRYPOINT="./server/index.mjs"
		SSR_WRAPPER="/usr/local/server/src/server-nuxt.mjs"
	fi

	# If there's an SSR wrapper, create a synthetic entry that traces both the
	# framework entrypoint and the wrapper's third-party dependencies.
	# We import the packages directly (not the wrapper file) so they resolve
	# from outputDir where node_modules lives.
	if [ -n "$SSR_WRAPPER" ] && [ -n "$ENTRYPOINT" ]; then
		if [ "$ENTRYPOINT" != "./.nft-entry.mjs" ]; then
			echo "import \"$ENTRYPOINT\";" >./.nft-entry.mjs
		fi
		# Include user's custom server file if present (e.g. Remix custom Express server)
		for f in ./server.mjs ./server.js; do
			if [ -e "$f" ] && [ "$f" != "$ENTRYPOINT" ]; then
				echo "import \"$f\";" >>./.nft-entry.mjs
			fi
		done
		grep -o 'from "[^"]*"' "$SSR_WRAPPER" | cut -d'"' -f2 | while read -r dep; do
			[[ "$dep" == ./* || "$dep" == ../* || "$dep" == /* ]] && continue
			echo "import \"$dep\";"
		done >>./.nft-entry.mjs
		ENTRYPOINT="./.nft-entry.mjs"
	fi

	if [ -z "$ENTRYPOINT" ]; then
		return 0 2>/dev/null || exit 0
	fi

	# Run NFT — capture exit code without triggering set -e
	NFT_EXIT=0
	NODE_PATH=/usr/local/server/node_modules node /usr/local/server/src/nft.mjs "$ENTRYPOINT" "$OUTPUT_DIR" || NFT_EXIT=$?

	NFT_MANIFEST="$OUTPUT_DIR/.nft-files"

	NEEDED_FILES=()

	if [ "$NFT_EXIT" -eq 0 ] && [ -f "$NFT_MANIFEST" ]; then
		# Read null-delimited manifest, filter to node_modules/ entries only
		mapfile -d '' ALL_FILES <"$NFT_MANIFEST"
		for file in "${ALL_FILES[@]}"; do
			if [[ "$file" == node_modules/* ]]; then
				NEEDED_FILES+=("$file")
			fi
		done
	elif [ -d "$OUTPUT_DIR/.next" ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m NFT failed (exit $NFT_EXIT), falling back to Next.js .nft.json traces. \e[0m"
	else
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m NFT failed (exit $NFT_EXIT), skipping pruning. \e[0m"
		rm -f "$OUTPUT_DIR/.nft-entry.mjs"
		return 0 2>/dev/null || exit 0
	fi

	# Next.js bundles pre-compiled dependencies loaded via dynamic require()
	# that NFT cannot trace — always keep them.
	if [ -d "$OUTPUT_DIR/.next" ] && [ -d "$OUTPUT_DIR/node_modules/next/dist/compiled" ]; then
		while IFS= read -r -d '' f; do
			NEEDED_FILES+=("$f")
		done < <(cd "$OUTPUT_DIR" && find node_modules/next/dist/compiled -type f -print0)
	fi

	# Keep @next/swc-* native binaries so Next.js doesn't download them at startup.
	if [ -d "$OUTPUT_DIR/.next" ]; then
		for swcdir in "$OUTPUT_DIR"/node_modules/@next/swc-*; do
			[ -d "$swcdir" ] || continue
			while IFS= read -r -d '' f; do
				NEEDED_FILES+=("$f")
			done < <(cd "$OUTPUT_DIR" && find "node_modules/@next/$(basename "$swcdir")" -type f -print0)
		done
	fi

	# Next.js generates .nft.json trace files during build for each server bundle.
	# These contain accurate dependency info that our NFT re-trace may miss
	# (webpack-compiled externals use patterns NFT can't always follow).
	if [ -d "$OUTPUT_DIR/.next" ]; then
		while IFS= read -r -d '' dep; do
			NEEDED_FILES+=("$dep")
		done < <(find "$OUTPUT_DIR/.next" -name '*.nft.json' -print0 | node /usr/local/server/src/nft-nextjs.mjs "$OUTPUT_DIR" 2>/dev/null)
	fi

	if [ "${#NEEDED_FILES[@]}" -eq 0 ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m No node_modules dependencies traced — pruning all of node_modules \e[0m"
	fi

	# Delete everything in node_modules not referenced by the trace
	(cd "$OUTPUT_DIR" && find node_modules -type f -o -type l) | sort >/tmp/.nft-all
	printf '%s\n' "${NEEDED_FILES[@]}" | sort -u >/tmp/.nft-keep
	comm -23 /tmp/.nft-all /tmp/.nft-keep | tr '\n' '\0' | xargs -0 rm -f
	find "$OUTPUT_DIR/node_modules" -type d -empty -delete

	SIZE_AFTER=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Pruned node_modules: ${SIZE_BEFORE}MB → ${SIZE_AFTER:-0}MB \e[0m"

	# Clean up
	rm -f /tmp/.nft-all /tmp/.nft-keep "$NFT_MANIFEST" "$OUTPUT_DIR/.nft-entry.mjs"

	export OPEN_RUNTIMES_CLEANUP="nft"
	return 0 2>/dev/null || exit 0
fi

# ─── modclean (default) ──────────────────────────────────────────────────────
if [[ "${OPEN_RUNTIMES_MODCLEAN,,}" != "disabled" ]]; then
	modclean --patterns default:safe --no-progress --run
	export OPEN_RUNTIMES_CLEANUP="modclean"
fi
