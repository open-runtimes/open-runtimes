#!/bin/bash
# Shared code extraction logic with sidecar support
# Source this script from lifecycle/start.sh to handle code extraction

# Prepare telemetry
mkdir -p /mnt/telemetry

# Detect archive compression by reading the first 4 magic bytes. Legacy tar
# builds pack as code.tar.gz regardless of actual compression, so the filename
# can't be trusted.
detect_archive_format() {
	local magic
	magic=$(head -c4 "$1" 2>/dev/null | od -An -vtx1 2>/dev/null | tr -d ' \n')
	case "$magic" in
	68737173) echo "squashfs" ;;
	1f8b*) echo "gzip" ;;
	28b52ffd) echo "zstd" ;;
	*) echo "tar" ;;
	esac
}

# Extract one archive into a destination dir, picking the right decompressor
# from detected format.
extract_archive() {
	local archive="$1"
	local dest="$2"
	local format
	format=$(detect_archive_format "$archive")
	case "$format" in
	squashfs) unsquashfs -q -f -d "$dest" "$archive" ;;
	# Prefer igzip, then pigz — both much faster than tar's built-in zlib
	gzip)
		if command -v igzip >/dev/null 2>&1; then
			(set -o pipefail && igzip -dc "$archive" | tar -xf - -C "$dest")
		elif command -v pigz >/dev/null 2>&1; then
			(set -o pipefail && pigz -dc "$archive" | tar -xf - -C "$dest")
		else
			tar -xzf "$archive" -C "$dest"
		fi
		;;
	# Subshell with pipefail so a zstd error (corrupt archive, missing
	# binary) surfaces as a non-zero exit instead of being masked by tar
	# choking on a truncated stream.
	zstd) (set -o pipefail && zstd -dc "$archive" | tar -xf - -C "$dest") ;;
	*) tar -xf "$archive" -C "$dest" ;;
	esac
}

# Locate the build archive in /mnt/code and extract it to dest. Squashfs is
# preferred when present, while legacy tar artifacts remain supported.
extract_code_archive() {
	local dest="$1"
	if [ -f /mnt/code/code.sqfs ]; then
		extract_archive /mnt/code/code.sqfs "$dest"
	elif [ -f /mnt/code/code.tar ]; then
		extract_archive /mnt/code/code.tar "$dest"
	elif [ -f /mnt/code/code.tar.gz ]; then
		extract_archive /mnt/code/code.tar.gz "$dest"
	elif [ -f /mnt/code/code.gz ]; then
		extract_archive /mnt/code/code.gz "$dest"
	else
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code archive not found. \e[0m"
		exit 1
	fi
}

# Where the code is, declared by the orchestrator on the runtime container:
# OPEN_RUNTIMES_CODE_PATH names a directory already holding the servable tree
# (symlinked in, or served in place when it is the function path itself), or an
# archive file to extract into the function path. Unset falls back to the
# legacy signals: the .extracted marker on the code volume, else an archive
# hunted down in /mnt/code.
code_path="$OPEN_RUNTIMES_CODE_PATH"
if [ -z "$code_path" ] && [ -f "/mnt/code/.extracted" ]; then
	code_path="/mnt/code"
fi

if [ -n "$code_path" ] && [ ! -e "$code_path" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code path $code_path not found. \e[0m"
	exit 1
fi

if [ -f "$code_path" ]; then
	# An archive: extract it into the function path.
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction started. \e[0m"

	start=$(awk '{print $1}' /proc/uptime)
	extract_archive "$code_path" /usr/local/server/src/function

	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	echo "extract=$elapsed" >>/mnt/telemetry/timings.txt
elif [ "$code_path" = "/usr/local/server/src/function" ]; then
	# The servable tree already sits at the function path (e.g. the
	# orchestrator mounted it there) — nothing to clear, link, or extract.
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code mounted in place, skipping extraction. \e[0m"
	echo "symlink=0" >>/mnt/telemetry/timings.txt
elif [ -n "$code_path" ]; then
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code already extracted, skipping extraction. \e[0m"

	start=$(awk '{print $1}' /proc/uptime)
	shopt -s dotglob

	# Clear stub files baked into the runtime image at this path. Some
	# runtimes (e.g. Rust) ship a placeholder Cargo.toml + lib.rs here so
	# their workspace compiles. Without this, `ln -s` would collide and
	# force a full re-extraction on every cold start.
	rm -rf /usr/local/server/src/function/*

	symlink_failed=false
	for item in "$code_path"/*; do
		# Skip archive files and marker
		case "$(basename "$item")" in
		code.sqfs | code.tar | code.tar.gz | code.gz | .extracted) continue ;;
		esac
		ln -s "$item" /usr/local/server/src/function/ || symlink_failed=true
	done

	if [ "$symlink_failed" = true ]; then
		echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Symlink failed, falling back to extraction. \e[0m"
		rm -rf /usr/local/server/src/function/*
		extract_code_archive /usr/local/server/src/function
	fi
	shopt -u dotglob

	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	if [ "$symlink_failed" = true ]; then
		echo "extract=$elapsed" >>/mnt/telemetry/timings.txt
	else
		echo "symlink=$elapsed" >>/mnt/telemetry/timings.txt
	fi
else
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction started. \e[0m"

	# Extract code from mounted volume to function folder
	start=$(awk '{print $1}' /proc/uptime)
	extract_code_archive /usr/local/server/src/function

	end=$(awk '{print $1}' /proc/uptime)
	elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
	echo "extract=$elapsed" >>/mnt/telemetry/timings.txt
fi

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code extraction finished. \e[0m"

# Apply env vars from build step
set -o allexport
. /usr/local/server/src/function/.open-runtimes
set +o allexport
