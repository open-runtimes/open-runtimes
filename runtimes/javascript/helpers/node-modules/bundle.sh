#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepares what SSR bundles ship from the build root: package.json and
# node_modules. Only node_modules is shipped, so links in it must stay inside.
BUILD_DIR=/usr/local/build
ROOT_MODULES="$BUILD_DIR/node_modules"

# The app is the nearest package.json above the output directory. In
# monorepos it is a subfolder of the build root.
OUTPUT_PARENT="$(dirname "${OPEN_RUNTIMES_OUTPUT_DIRECTORY:-.}")"
[[ "$OUTPUT_PARENT" == /* ]] || OUTPUT_PARENT="$BUILD_DIR/$OUTPUT_PARENT"
PROJECT_DIR="$(cd "$OUTPUT_PARENT" 2>/dev/null && pwd)" || PROJECT_DIR="$BUILD_DIR"
[[ "$PROJECT_DIR" == "$BUILD_DIR"/* ]] || PROJECT_DIR="$BUILD_DIR"
while [ "$PROJECT_DIR" != "$BUILD_DIR" ] && [ ! -f "$PROJECT_DIR/package.json" ]; do
	PROJECT_DIR="$(dirname "$PROJECT_DIR")"
done

if [ "$PROJECT_DIR" != "$BUILD_DIR" ]; then
	# The app's manifest ("type", dependencies) is the one to ship. Root
	# dependencies stay listed, as the pack hook traces every dependency.
	MANIFEST='const fs = require("fs"); const [root, app] = process.argv.slice(1).map((file) => JSON.parse(fs.readFileSync(file))); app.dependencies = { ...root.dependencies, ...app.dependencies }; fs.writeFileSync(process.argv[1], JSON.stringify(app, null, 2))'
	node -e "$MANIFEST" "$BUILD_DIR/package.json" "$PROJECT_DIR/package.json" 2>/dev/null || cp "$PROJECT_DIR/package.json" "$BUILD_DIR/package.json"

	# The app was built against its own node_modules, so its entries replace
	# root entries of the same name
	for entry in "$PROJECT_DIR"/node_modules/* "$PROJECT_DIR"/node_modules/@*/*; do
		name="${entry#"$PROJECT_DIR"/node_modules/}"
		# Package manager metadata and stores; generated dirs such as .prisma move
		case "$name" in .bin | .pnpm | .bun | .modules.yaml | .package-lock.json | .yarn-state.yml | .yarn-integrity | .cache | .vite) continue ;; esac
		# Scope directories are walked by the second glob
		[[ "$name" == @* && "$name" != */* && ! -L "$entry" ]] && continue
		target="$(readlink -f "$entry")" && [ -e "$target" ] || continue
		[ "$(readlink -f "$ROOT_MODULES/$name" 2>/dev/null)" = "$target" ] && continue
		mkdir -p "$(dirname "$ROOT_MODULES/$name")"
		rm -rf "${ROOT_MODULES:?}/$name"
		if [ -L "$entry" ]; then
			# Made relative or copied below
			ln -s "$target" "$ROOT_MODULES/$name"
		else
			mv "$entry" "$ROOT_MODULES/$name"
		fi
	done
fi

# Absolute links into the store (pnpm, bun isolated) become relative
for entry in "$ROOT_MODULES"/* "$ROOT_MODULES"/@*/*; do
	[ -L "$entry" ] && [[ "$(readlink "$entry")" == /* ]] || continue
	target="$(readlink -f "$entry")" && [[ "$target" == "$ROOT_MODULES/"* ]] || continue
	name="${entry#"$ROOT_MODULES"/}"
	slashes="${name//[!\/]/}"
	ln -sfn "${slashes//\//../}${target#"$ROOT_MODULES"/}" "$entry"
done

# Workspace packages live outside node_modules. Copy the ones the app uses,
# directly or through other workspace packages. devDependencies count for the
# app itself, as configs (next.config) load them at runtime.
DEPENDENCIES='const [file, ...fields] = process.argv.slice(1); const p = require(file); console.log(fields.flatMap((field) => Object.keys(p[field] || {})).join("\n"))'
mapfile -t pending < <(node -e "$DEPENDENCIES" "$BUILD_DIR/package.json" dependencies optionalDependencies peerDependencies devDependencies 2>/dev/null || true)
for ((i = 0; i < ${#pending[@]}; i++)); do
	name="${pending[$i]}"
	entry="$ROOT_MODULES/$name"
	# Copied ones are directories by now, so names seen twice are skipped
	[ -n "$name" ] && [ -L "$entry" ] || continue
	target="$(readlink -f "$entry")" && [ -e "$target" ] || continue
	# Inside node_modules already, the build root (a workspace root package) or the app
	[[ "$target" == "$ROOT_MODULES/"* || "$ROOT_MODULES" == "$target"/* || "$target" == "$PROJECT_DIR" ]] && continue
	mapfile -t needed < <(node -e "$DEPENDENCIES" "$target/package.json" dependencies optionalDependencies peerDependencies 2>/dev/null || true)
	rm "$entry"
	cp -R "$target" "$entry"
	for link in "$entry"/node_modules/* "$entry"/node_modules/@*/*; do
		[ -L "$link" ] || continue
		dependency="$(readlink -f "$target/${link#"$entry"/}")" && [ -e "$dependency" ] || continue
		rm "$link"
		if [[ "$dependency" == "$ROOT_MODULES/"* ]]; then
			relative="${link#"$ROOT_MODULES"/}"
			slashes="${relative//[!\/]/}"
			ln -s "${slashes//\//../}${dependency#"$ROOT_MODULES"/}" "$link"
		else
			# Another workspace package, resolved from its copy at the root unless
			# only needed for development
			name="${link#"$entry"/node_modules/}"
			[[ " ${needed[*]} " == *" $name "* ]] || continue
			if [ ! -e "$ROOT_MODULES/$name" ] && [ ! -L "$ROOT_MODULES/$name" ]; then
				mkdir -p "$(dirname "$ROOT_MODULES/$name")"
				ln -s "$dependency" "$ROOT_MODULES/$name"
			fi
		fi
	done
	pending+=("${needed[@]}")
done

# Absolute links left over point at workspace packages the app does not use
for entry in "$ROOT_MODULES"/* "$ROOT_MODULES"/@*/*; do
	[ -L "$entry" ] && [[ "$(readlink "$entry")" == /* ]] || continue
	rm "$entry"
done

# pnpm and bun (isolated linker) link only direct dependencies at the top level
# and hoist the rest into a private node_modules inside their store. Runtime
# servers and Nitro output import framework dependencies (@remix-run/express,
# vue-bundle-renderer) directly, so link those too.
for store in .pnpm .bun; do
	for entry in "$ROOT_MODULES/$store"/node_modules/* "$ROOT_MODULES/$store"/node_modules/@*/*; do
		[ -e "$entry" ] || [ -L "$entry" ] || continue
		name="${entry#"$ROOT_MODULES/$store"/node_modules/}"
		[[ "$name" == .* ]] && continue
		[[ "$name" == @* && "$name" != */* && ! -L "$entry" ]] && continue
		[ -e "$ROOT_MODULES/$name" ] || [ -L "$ROOT_MODULES/$name" ] && continue
		# Hoisted workspace packages point outside node_modules
		[[ "$(readlink -f "$entry")" == "$ROOT_MODULES/"* ]] || continue
		# The runtime servers' own imports resolve to the image's copies
		case "$name" in express | serve-static | srvx) continue ;; esac
		slashes="${name//[!\/]/}"
		mkdir -p "$(dirname "$ROOT_MODULES/$name")"
		ln -s "${slashes//\//../}$store/node_modules/$name" "$ROOT_MODULES/$name"
	done
done
