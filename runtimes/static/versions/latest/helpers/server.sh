#!/bin/bash
set -e

# Basic auth
if [ -n "$OPEN_RUNTIMES_SECRET" ]; then
    export AUTH=$(caddy hash-password --plaintext "$OPEN_RUNTIMES_SECRET")
else
    export AUTH="disabled"
fi

# SPA behaviour
if [ -n "$OPEN_RUNTIMES_STATIC_FALLBACK" ]; then
    export PAGE_FALLBACK="/$OPEN_RUNTIMES_STATIC_FALLBACK"
else
    export PAGE_FALLBACK="/404.html"
fi

# 404 page design fallback
if [ ! -e "/mnt/resources/404.html" ]; then
    cp /usr/local/server/404.html /mnt/resources/404.html
fi

# Start Caddy
caddy run --config /usr/local/server/Caddyfile
