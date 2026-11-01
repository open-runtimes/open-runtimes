#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepares the root node_modules, which SSR bundles ship.
#
# In monorepos the app (nearest package.json above the output directory) can
# have its own node_modules; move its entries into the root one.
PROJECT_DIR="$(cd "/usr/local/build/$(dirname "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-.}")" && pwd)"
while [ "$PROJECT_DIR" != "/usr/local/build" ] && [ ! -f "$PROJECT_DIR/package.json" ]; do
	PROJECT_DIR="$(dirname "$PROJECT_DIR")"
done

ROOT_MODULES=/usr/local/build/node_modules

if [ "$PROJECT_DIR" != "/usr/local/build" ]; then
	# Symlinks into the root store (pnpm, bun isolated) are rewritten relative to
	# their new location; other symlinks (workspace packages) are copied.
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

# pnpm links only direct dependencies at the top level and hoists the rest into
# .pnpm/node_modules. Runtime servers and Nitro output import framework
# dependencies (express, vue-bundle-renderer) directly, so link those too.
for entry in "$ROOT_MODULES"/.pnpm/node_modules/* "$ROOT_MODULES"/.pnpm/node_modules/@*/*; do
	[ -e "$entry" ] || [ -L "$entry" ] || continue
	name="${entry#"$ROOT_MODULES"/.pnpm/node_modules/}"
	[[ "$name" == @* && "$name" != */* && ! -L "$entry" ]] && continue
	[ -e "$ROOT_MODULES/$name" ] || [ -L "$ROOT_MODULES/$name" ] && continue
	prefix=""
	[[ "$name" == */* ]] && prefix="../"
	mkdir -p "$(dirname "$ROOT_MODULES/$name")"
	ln -s "$prefix.pnpm/node_modules/$name" "$ROOT_MODULES/$name"
done
