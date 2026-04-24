#!/bin/bash
# Prune node_modules to reduce build output size
# Strategies: NFT (opt-in tree-shaking) or modclean (default junk removal)

export OPEN_RUNTIMES_CLEANUP="none"

# Determine output directory
OUTPUT_DIR="/usr/local/build"
if [ -n "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-}" ]; then
	OUTPUT_DIR="/usr/local/build/$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Skip if no node_modules (not a JS dependency build)
if [ ! -d "$OUTPUT_DIR/node_modules" ]; then
	return 0 2>/dev/null || exit 0
fi

cd "$OUTPUT_DIR" || exit

# ─── NFT (opt-in) ────────────────────────────────────────────────────────────
if [[ "${OPEN_RUNTIMES_NFT:-}" == "enabled" ]]; then

	SIZE_BEFORE=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)

	NEEDED_FILES=()

	if [ -d "./.next" ]; then
		# Next.js NFT is intentionally skipped: native .nft.json traces miss
		# server-wrapper and next.config imports. Use default modclean instead.
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m Next.js NFT cleanup skipped, falling back to modclean. \e[0m"
		if [[ "${OPEN_RUNTIMES_MODCLEAN,,}" != "disabled" ]]; then
			modclean --patterns default:safe --no-progress --run
			export OPEN_RUNTIMES_CLEANUP="modclean"
		fi
		return 0 2>/dev/null || exit 0
	else
		# Generic frameworks — detect entrypoint and run NFT
		ENTRYPOINT=""
		SSR_WRAPPER=""

		if [ -n "${OPEN_RUNTIMES_START_COMMAND:-}" ]; then
			if [[ "$OPEN_RUNTIMES_START_COMMAND" =~ (node|bun|deno[[:space:]]+run)[[:space:]]+([^[:space:]]+\.[cm]?[jt]s) ]]; then
				ENTRYPOINT="${BASH_REMATCH[2]}"
			fi
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
			echo "import \"$ENTRYPOINT\";" >./.nft-entry.mjs
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

			# Include declared production dependencies so that native/binary packages
			# and bundler-externalized modules are traced even when the framework bundler
			# (Nitro/Vite) has already inlined JS deps.
			if [ -f "$OUTPUT_DIR/package.json" ]; then
				node -e "
					const pkg = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf-8'));
					for (const dep of Object.keys(pkg.dependencies || {})) {
						process.stdout.write('import \"' + dep + '\";\n');
					}
				" "$OUTPUT_DIR/package.json" >>./.nft-entry.mjs
			fi

			ENTRYPOINT="./.nft-entry.mjs"
		fi

		if [ -z "$ENTRYPOINT" ]; then
			return 0 2>/dev/null || exit 0
		fi

		# Run NFT — capture exit code without triggering set -e
		NFT_EXIT=0
		NODE_PATH=/usr/local/server/node_modules node /usr/local/server/src/nft.mjs "$ENTRYPOINT" "$OUTPUT_DIR" || NFT_EXIT=$?

		NFT_MANIFEST="$OUTPUT_DIR/.nft-files"

		if [ "$NFT_EXIT" -eq 0 ] && [ -f "$NFT_MANIFEST" ]; then
			# Read null-delimited manifest, filter to node_modules/ entries only
			mapfile -d '' ALL_FILES <"$NFT_MANIFEST"
			for file in "${ALL_FILES[@]}"; do
				if [[ "$file" == node_modules/* ]]; then
					NEEDED_FILES+=("$file")
				fi
			done
		else
			echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m NFT failed (exit $NFT_EXIT), skipping pruning. \e[0m"
			rm -f "$OUTPUT_DIR/.nft-entry.mjs"
			return 0 2>/dev/null || exit 0
		fi
	fi

	if [ "${#NEEDED_FILES[@]}" -eq 0 ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m No node_modules dependencies traced — skipping pruning \e[0m"
		return 0 2>/dev/null || exit 0
	fi

	# Delete everything in node_modules not referenced by the trace
	(cd "$OUTPUT_DIR" && find node_modules -type f -o -type l) | sort >/tmp/.nft-all
	printf '%s\n' "${NEEDED_FILES[@]}" | sort -u >/tmp/.nft-keep
	comm -23 /tmp/.nft-all /tmp/.nft-keep | tr '\n' '\0' | xargs -0 rm -f
	find "$OUTPUT_DIR/node_modules" -type d -empty -delete

	SIZE_AFTER=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Pruned node_modules: ${SIZE_BEFORE}MB → ${SIZE_AFTER:-0}MB \e[0m"

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
