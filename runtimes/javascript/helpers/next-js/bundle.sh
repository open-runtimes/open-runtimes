#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Subdirectories of .next/cache that are pure build-time artifacts and never
# read at runtime. Safe to drop before packaging.
#   webpack — webpack persistent build cache (often hundreds of MB)
#   swc     — SWC compiler cache
# Not listed (intentionally kept): fetch-cache, images, server, use-cache —
# these are populated by `next build` for ISR / RSC / image optimization and
# removing them changes runtime behavior.
NEXT_BUILD_ONLY_CACHE_DIRS=("webpack" "swc")

for dir in "${NEXT_BUILD_ONLY_CACHE_DIRS[@]}"; do
	if [ -d "./cache/$dir" ]; then
		SIZE=$(du -sh "./cache/$dir" 2>/dev/null | cut -f1)
		rm -rf "./cache/$dir"
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Pruned build-only cache .next/cache/$dir (${SIZE:-?}) \e[0m"
	fi
done

# Next.js project directory, parent of the output directory. Differs from the
# build root in monorepos (e.g. output "./apps/web/.next" -> "./apps/web").
PROJECT_DIR="/usr/local/build/$(dirname "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-.}")"

# Move project node_modules entries into the root node_modules, so dependencies
# installed only in the project directory are bundled too. Symlinks into the
# root store (pnpm, bun isolated) are rewritten relative to their new location;
# other symlinks (workspace packages) are copied.
merge_node_modules() {
	local source="$1" destination="$2" prefix="$3" entry name target
	mkdir -p "$destination"
	for entry in "$source"/*; do
		[ -e "$entry" ] || [ -L "$entry" ] || continue
		name="$(basename "$entry")"
		[ "$name" = ".bin" ] && continue
		if [[ "$name" == @* ]] && [ ! -L "$entry" ]; then
			merge_node_modules "$entry" "$destination/$name" "../$prefix"
			continue
		fi
		target="$(readlink -f "$entry")"
		# Already provided by the root node_modules
		if [[ "$target" == "$destination/$name" || "$target" == "$destination/$name/"* ]]; then
			continue
		fi
		rm -rf "${destination:?}/$name"
		if [ -L "$entry" ]; then
			if [[ "$target" == /usr/local/build/node_modules/* ]]; then
				ln -s "$prefix${target#/usr/local/build/node_modules/}" "$destination/$name"
			else
				cp -R "$target" "$destination/$name"
			fi
		else
			mv "$entry" "$destination/$name"
		fi
	done
}

WEBPACK_ENTRYPOINT="./server/webpack-runtime.js"
TURBOPACK_ENTRYPOINT="./turbopack"
STANDALONE_ENTRYPOINT="./standalone/server.js"

if [ -e "$STANDALONE_ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Detected standalone Next.js build. \e[0m"

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	mkdir -p /tmp/.opr-tmp
	mv "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" /tmp/.opr-tmp

	mkdir -p "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"

	mv /tmp/.opr-tmp/.next/standalone/* ./

	# Only copy public and static, rest is there
	if [ -d "$PROJECT_DIR/public/" ]; then
		mkdir -p ./public
		mv "$PROJECT_DIR"/public/* ./public/
	fi

	if [ -d "/tmp/.opr-tmp/.next/static/" ]; then
		mkdir -p ./.next/static
		mv /tmp/.opr-tmp/.next/static/* ./.next/static/
	fi

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"

elif [ -e "$WEBPACK_ENTRYPOINT" ] || [ -e "$TURBOPACK_ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	if [ "$PROJECT_DIR" != "/usr/local/build/." ] && [ -d "$PROJECT_DIR/node_modules/" ]; then
		merge_node_modules "$PROJECT_DIR/node_modules" /usr/local/build/node_modules ""
	fi

	mkdir -p /tmp/.opr-tmp
	mv "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" /tmp/.opr-tmp

	mkdir -p "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
	mv /tmp/.opr-tmp/* .next/

	# Copy over public folder, package.json, next config, and node_modules
	if [ -d "$PROJECT_DIR/public/" ]; then
		mv "$PROJECT_DIR/public/" ./public/
	fi

	mv "$PROJECT_DIR"/package*.json ./
	# next.config is optional
	if compgen -G "$PROJECT_DIR/next.config.*" >/dev/null; then
		mv "$PROJECT_DIR"/next.config.* ./
	fi
	mv /usr/local/build/node_modules/ ./node_modules/

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"
fi
