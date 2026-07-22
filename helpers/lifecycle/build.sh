#!/bin/bash
# Build lifecycle: stage -> build-prepare hook -> install command -> compile
# hook -> pack hook -> archive -> manifest. Per-runtime steps live in
# /usr/local/server/hooks/.
# Fail build if any command fails
set -e
shopt -s dotglob

. /usr/local/server/helpers/lifecycle/lib.sh

# --- Build cache: restore package-manager caches before the build runs ---

. /usr/local/server/helpers/build-cache-env.sh
bash /usr/local/server/helpers/build-cache-restore.sh || true

# --- Stage: copy source code into the build directory ---

opr_log "Environment preparation started."

# Check if source directory exists and has files
INPUT_DIR="${OPEN_RUNTIMES_BUILD_INPUT_DIR:-/mnt/code}"
if [ ! -d "$INPUT_DIR" ] || [ -z "$(ls -A "$INPUT_DIR" 2>/dev/null)" ]; then
	opr_error "Error: No source code found. Ensure your source isn't empty."
	exit 1
fi

# Copy from mounted volume to temporary folder
cp -R "$INPUT_DIR"/* /usr/local/build

# Enter build folder
cd /usr/local/build

opr_run_hook build-prepare

opr_log "Environment preparation finished."

# --- Install: user-provided build command ---

# Enter build folder
cd /usr/local/build

opr_log "Build command execution started."

bash -c "$1"

opr_log "Build command execution finished."

# --- Compile ---

cd /usr/local/server

opr_run_hook compile

# --- Pack ---

opr_log "Build packaging started."

opr_run_hook pack

# --- Archive: prepare tar to use for starting the runtime ---

cd /usr/local/build/

# Check if the output directory is empty
# shellcheck disable=SC2086 # Intentional: unquoted variable allows ls -A to work when variable is empty
if [ -z "$(ls -A $OPEN_RUNTIMES_OUTPUT_DIRECTORY 2>/dev/null)" ]; then
	opr_error "Error: No build output found. Ensure your output directory isn't empty."
	exit 1
fi

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
	cd "$OPEN_RUNTIMES_OUTPUT_DIRECTORY"
fi

# Store build metadata. Will be used during start process
touch .open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" >.open-runtimes
echo "OPEN_RUNTIMES_CLEANUP=${OPEN_RUNTIMES_CLEANUP:-none}" >>.open-runtimes

. /usr/local/server/helpers/lifecycle/compression.sh

echo "OPEN_RUNTIMES_COMPRESSION=$COMPRESSION_METHOD" >>.open-runtimes

OUTPUT_DIR="${OPEN_RUNTIMES_BUILD_OUTPUT_DIR:-/mnt/code}"
mkdir -p "$OUTPUT_DIR"
if [ "$COMPRESSION_METHOD" = "skip" ]; then
	# No archive: drop the raw build output into the output dir and mark it
	# pre-extracted so start-up symlinks it directly (like a sidecar would).
	cp -R . "$OUTPUT_DIR/"
	touch "$OUTPUT_DIR/.extracted"
elif [ "$COMPRESSION_METHOD" = "none" ]; then
	tar --exclude code.sqfs --exclude code.tar --exclude code.tar.gz --exclude code.gz -cf "$OUTPUT_DIR/code.tar.gz" .
elif [ "$COMPRESSION_METHOD" = "squashfs" ]; then
	processors=$(nproc 2>/dev/null || echo 1)
	squashfs_log=$(mktemp)
	trap 'rm -f "$squashfs_log"' EXIT
	if mksquashfs . "$OUTPUT_DIR/code.sqfs" -comp lz4 -b 1M -noappend -no-xattrs -no-progress -processors "$processors" -wildcards -e code.sqfs code.tar code.tar.gz code.gz >"$squashfs_log" 2>&1; then
		rm -f "$squashfs_log"
		trap - EXIT
	else
		status=$?
		cat "$squashfs_log" >&2
		exit "$status"
	fi
elif [ "$COMPRESSION_METHOD" = "zstd" ]; then
	tar --exclude code.sqfs --exclude code.tar --exclude code.tar.gz --exclude code.gz -cf - . | zstd -qf -o "$OUTPUT_DIR/code.tar.gz"
else
	tar --exclude code.sqfs --exclude code.tar --exclude code.tar.gz --exclude code.gz -zcf "$OUTPUT_DIR/code.tar.gz" .
fi

opr_log "Build packaging finished."

# --- Manifest: describe the build output for the orchestrator (opt-in) ---

bash /usr/local/server/helpers/build-manifest.sh || true

# --- Build cache: persist package-manager caches for the next build ---

bash /usr/local/server/helpers/build-cache-save.sh || true

opr_success "Build finished."
