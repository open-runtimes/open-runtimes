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
PROJECT_PATH="$(dirname "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-.}")"
PROJECT_PATH="${PROJECT_PATH#./}"
PROJECT_DIR="/usr/local/build/$PROJECT_PATH"

WEBPACK_ENTRYPOINT="./server/webpack-runtime.js"
TURBOPACK_ENTRYPOINT="./turbopack"
# Monorepo standalone builds keep the tree: standalone/apps/web/server.js
STANDALONE_ENTRYPOINT="./standalone/$PROJECT_PATH/server.js"

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
		mkdir -p "./$PROJECT_PATH/public"
		mv "$PROJECT_DIR"/public/* "./$PROJECT_PATH/public/"
	fi

	if [ -d "/tmp/.opr-tmp/.next/static/" ]; then
		mkdir -p "./$PROJECT_PATH/.next/static"
		mv /tmp/.opr-tmp/.next/static/* "./$PROJECT_PATH/.next/static/"
	fi

	# The server starts ./server.js; import() loads the project's server.js
	# whether the root package.json is CommonJS or ESM
	if [ "$PROJECT_PATH" != "." ]; then
		echo "import('./$PROJECT_PATH/server.js');" >./server.js
	fi

	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR finished. \e[0m"

elif [ -e "$WEBPACK_ENTRYPOINT" ] || [ -e "$TURBOPACK_ENTRYPOINT" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Bundling for SSR started. \e[0m"

	cd /usr/local/build

	if [ "$PROJECT_PATH" != "." ] && [ -d "$PROJECT_DIR/node_modules/" ]; then
		# Move project node_modules entries into the root node_modules, so
		# dependencies installed only in the project are bundled too. Symlinks
		# into the root store (pnpm, bun isolated) are rewritten relative to
		# their new location; other symlinks (workspace packages) are copied.
		ROOT_MODULES=/usr/local/build/node_modules
		for entry in "$PROJECT_DIR"/node_modules/* "$PROJECT_DIR"/node_modules/@*/*; do
			[ -e "$entry" ] || [ -L "$entry" ] || continue
			name="${entry#"$PROJECT_DIR"/node_modules/}"
			[ "$name" = ".bin" ] && continue
			# Scope directories are walked by the second glob
			[[ "$name" == @* && "$name" != */* && ! -L "$entry" ]] && continue
			target="$(readlink -f "$entry")"
			# Already provided by the root node_modules
			[[ "$target" == "$ROOT_MODULES/$name" || "$target" == "$ROOT_MODULES/$name/"* ]] && continue
			mkdir -p "$(dirname "$ROOT_MODULES/$name")"
			rm -rf "${ROOT_MODULES:?}/$name"
			if [ ! -L "$entry" ]; then
				mv "$entry" "$ROOT_MODULES/$name"
			elif [[ "$target" == "$ROOT_MODULES/"* ]]; then
				prefix=""
				[[ "$name" == */* ]] && prefix="../"
				ln -s "$prefix${target#"$ROOT_MODULES"/}" "$ROOT_MODULES/$name"
			else
				cp -R "$target" "$ROOT_MODULES/$name"
			fi
		done
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
