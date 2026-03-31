#!/bin/bash
# Selects compression method. Source this after cd'ing to the directory to archive.
# Sets: COMPRESSION_METHOD

if [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "none" ]; then
    COMPRESSION_METHOD="none"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "zstd" ]; then
    COMPRESSION_METHOD="zstd"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "gzip" ]; then
    COMPRESSION_METHOD="gzip"
elif [ "$OPEN_RUNTIMES_BUILD_COMPRESSION" = "auto" ]; then
    TOTAL_SIZE_KB=$(du -sk . 2>/dev/null | cut -f1)
    TOTAL_SIZE_KB=${TOTAL_SIZE_KB:-0}

    if [ "$TOTAL_SIZE_KB" -lt 5120 ]; then
        # < 5MB: no compression
        COMPRESSION_METHOD="none"
    elif [ "$TOTAL_SIZE_KB" -lt 10240 ]; then
        # 5-10MB: gzip (edge decompresses with igzip)
        COMPRESSION_METHOD="gzip"
    else
        # >= 10MB: zstd (best decompression throughput)
        COMPRESSION_METHOD="zstd"
    fi

    echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Auto compression: ${COMPRESSION_METHOD} (size=${TOTAL_SIZE_KB}KB) \e[0m"
else
    # Default: gzip (backward compatible)
    COMPRESSION_METHOD="gzip"
fi
