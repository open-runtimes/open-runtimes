#!/bin/bash
# Replaces node_modules in the build output with a fresh production-only
# install from the lockfile, using the package manager the project installed
# with. The package manager cache is warm from the build's own install, so this
# takes seconds where tracing the full install (NFT) takes minutes.
#
# Usage: production-dependencies.sh <output directory>
# Exits non-zero, leaving node_modules untouched, whenever the project does not
# fit: no single lockfile, workspaces or local dependencies, Next.js, a package
# manager that is missing or unsupported, or an install that fails.

set -o pipefail
shopt -s dotglob

OUTPUT_DIR="$(realpath -m "$1")"
BUILD_DIR="/usr/local/build"

log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[33m $* \e[0m"
}

skip() {
	log "Production dependencies skipped: $*"
	exit 1
}

[ -f "$OUTPUT_DIR/package.json" ] || skip "no package.json"

# next.config and similar files load devDependencies at runtime
[ -d "$OUTPUT_DIR/.next" ] && skip "Next.js loads devDependencies at runtime"

# In monorepos the app is a subfolder of the build root and its lockfile
# covers other packages. Only a single package at the build root qualifies.
if [[ "$OUTPUT_DIR" == "$BUILD_DIR"/* ]]; then
	dir="$(dirname "$OUTPUT_DIR")"
	while [ "$dir" != "$BUILD_DIR" ]; do
		[ -f "$dir/package.json" ] && skip "app is not at the build root"
		dir="$(dirname "$dir")"
	done
fi

# Config files stay at the build root; SSR bundlers move package*.json into the
# output directory
find_file() {
	for base in "$OUTPUT_DIR" "$BUILD_DIR"; do
		if [ -e "$base/$1" ]; then
			echo "$base/$1"
			return 0
		fi
	done
	return 1
}

LOCKFILES=()
for lockfile in bun.lock bun.lockb pnpm-lock.yaml yarn.lock package-lock.json npm-shrinkwrap.json; do
	if find_file "$lockfile" >/dev/null; then
		LOCKFILES+=("$lockfile")
	fi
done

# Workspaces, local dependencies, the declared package manager
MANIFEST=$(node -e '
	const pkg = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
	const local = ["file:", "link:", "workspace:", "portal:"];
	const specs = ["dependencies", "optionalDependencies"].flatMap((field) => Object.values(pkg[field] || {}));
	if (pkg.workspaces) console.log("workspaces");
	else if (specs.some((spec) => local.some((prefix) => String(spec).startsWith(prefix)))) console.log("local");
	else console.log((pkg.packageManager || "").split("@")[0] || "none");
' "$OUTPUT_DIR/package.json") || skip "package.json is not valid JSON"

case "$MANIFEST" in
workspaces) skip "workspaces" ;;
local) skip "local dependencies" ;;
esac

if PNPM_WORKSPACE=$(find_file pnpm-workspace.yaml); then
	# A workspace file that only lists the root holds settings, not packages
	if grep -E "^packages:\s*\[" "$PNPM_WORKSPACE" | grep -q -v -E "^packages:\s*\[\s*['\"]?\.['\"]?\s*\]"; then
		skip "pnpm workspaces"
	fi
	if grep -v -E "^\s*-\s*['\"]?\.['\"]?\s*$" "$PNPM_WORKSPACE" | sed -n '/^packages:/,/^[^[:space:]-]/p' | grep -q -E '^\s*-'; then
		skip "pnpm workspaces"
	fi
fi

MANAGER=""
case "$MANIFEST" in
bun | pnpm | yarn | npm) MANAGER="$MANIFEST" ;;
none)
	[ "${#LOCKFILES[@]}" -eq 1 ] || skip "expected exactly one lockfile, found ${#LOCKFILES[@]}"
	case "${LOCKFILES[0]}" in
	bun.lock*) MANAGER="bun" ;;
	pnpm-lock.yaml) MANAGER="pnpm" ;;
	yarn.lock) MANAGER="yarn" ;;
	*) MANAGER="npm" ;;
	esac
	;;
*) skip "unsupported package manager $MANIFEST" ;;
esac

command -v "$MANAGER" >/dev/null 2>&1 || skip "$MANAGER is not installed"

case "$MANAGER" in
bun)
	LOCKFILE=$(find_file bun.lock || find_file bun.lockb) || skip "no bun lockfile"
	INSTALL=(bun install --production --frozen-lockfile --no-progress)
	;;
pnpm)
	LOCKFILE=$(find_file pnpm-lock.yaml) || skip "no pnpm lockfile"
	INSTALL=(pnpm install --prod --frozen-lockfile --config.confirmModulesPurge=false)
	;;
yarn)
	LOCKFILE=$(find_file yarn.lock) || skip "no yarn lockfile"
	# Yarn 2+ has no production install outside workspaces focus
	find_file .yarnrc.yml >/dev/null && skip "Yarn 2+"
	INSTALL=(yarn install --frozen-lockfile --production --non-interactive)
	;;
npm)
	# npm reads npm-shrinkwrap.json ahead of package-lock.json
	LOCKFILE=$(find_file npm-shrinkwrap.json || find_file package-lock.json) || skip "no npm lockfile"
	INSTALL=(npm ci --omit=dev --no-audit --no-fund)
	;;
esac

START=$(date +%s)
STAGE=$(mktemp -d "$BUILD_DIR/.opr-dependencies.XXXXXX")
trap 'rm -rf "$STAGE"' EXIT

# Root lifecycle scripts (prepare, postinstall) often call devDependencies, so
# the manifest goes in without them. Dependencies keep their own scripts.
node -e '
	const fs = require("fs");
	const pkg = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
	delete pkg.scripts;
	fs.writeFileSync(process.argv[2], JSON.stringify(pkg, null, 2));
' "$OUTPUT_DIR/package.json" "$STAGE/package.json" || skip "could not prepare package.json"
cp "$LOCKFILE" "$STAGE/"

for config in .npmrc .yarnrc bunfig.toml pnpm-workspace.yaml .pnpmfile.cjs patches; do
	if source=$(find_file "$config"); then
		cp -R "$source" "$STAGE/"
	fi
done

log "Installing production dependencies with $MANAGER."
if ! (cd "$STAGE" && CI=true NODE_ENV=production "${INSTALL[@]}"); then
	skip "$MANAGER install failed"
fi

[ -d "$STAGE/node_modules" ] || skip "install produced no node_modules"

# Every declared dependency must be in the new tree
MISSING=$(node -e '
	const fs = require("fs");
	const path = require("path");
	const pkg = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
	const missing = Object.keys(pkg.dependencies || {}).filter(
		(name) => !fs.existsSync(path.join(process.argv[2], name, "package.json")),
	);
	console.log(missing.join(" "));
' "$OUTPUT_DIR/package.json" "$STAGE/node_modules")
[ -z "$MISSING" ] || skip "missing after install: $MISSING"

# Directories that the build generated into node_modules (.prisma) are not in
# the lockfile. Carry them over; package manager metadata stays behind.
for entry in "$OUTPUT_DIR"/node_modules/.*; do
	name=$(basename "$entry")
	case "$name" in
	. | .. | .bin | .cache | .vite | .vite-temp | .pnpm | .bun | .modules.yaml | .package-lock.json | .yarn-state.yml | .yarn-integrity | .tmp) continue ;;
	esac
	[ -e "$STAGE/node_modules/$name" ] || mv "$entry" "$STAGE/node_modules/$name"
done

# Source maps, type declarations and docs are never loaded at runtime. They
# are a third of the files in a typical tree, and NFT leaves them out too.
find "$STAGE/node_modules" -type f \( -name '*.map' -o -name '*.d.ts' -o -name '*.d.mts' -o -name '*.d.cts' -o -name '*.md' \) -delete

SIZE_BEFORE=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)
mv "$OUTPUT_DIR/node_modules" "$STAGE/node_modules.full"
mv "$STAGE/node_modules" "$OUTPUT_DIR/node_modules"
SIZE_AFTER=$(du -sm "$OUTPUT_DIR/node_modules" 2>/dev/null | cut -f1)

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Production dependencies: ${SIZE_BEFORE}MB → ${SIZE_AFTER}MB in $(($(date +%s) - START))s \e[0m"
exit 0
