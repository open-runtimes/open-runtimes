#!/bin/bash
# Selects compression method. Source this after cd'ing to the directory to archive.
# Sets: COMPRESSION_METHOD

if [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "none" ]; then
	COMPRESSION_METHOD="none"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "squashfs" ]; then
	COMPRESSION_METHOD="squashfs"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "zstd" ]; then
	COMPRESSION_METHOD="zstd"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "gzip" ]; then
	COMPRESSION_METHOD="gzip"
else
	# Default (and "auto"): gzip (backward compatible)
	COMPRESSION_METHOD="gzip"
fi
