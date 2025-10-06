#!/bin/bash
set -e
shopt -s dotglob

# Write files to serve system routes
mkdir -p /usr/local/server/src/function/__opr
echo -n "OK" > /usr/local/server/src/function/__opr/health.txt
if [ -f "/mnt/telemetry/timings.txt" ]; then
    cp /mnt/telemetry/timings.txt /usr/local/server/src/function/__opr/timings.txt
fi

# Basic auth
if [ -z "$OPEN_RUNTIMES_SECRET" ]; then
    AUTH=""
else
    AUTH=$(echo -e "$OPEN_RUNTIMES_SECRET" | htpasswd -inBC10 "opr")
fi

# SPA behaviour
if [ -z "$OPEN_RUNTIMES_STATIC_FALLBACK" ]; then
    PAGE_FALLBACK=""
else
    PAGE_FALLBACK="/usr/local/server/src/function/$OPEN_RUNTIMES_STATIC_FALLBACK"
fi

# 404 page design fallback
if [ ! -e "/mnt/resources/404.html" ]; then
    cp /usr/local/server/404.html /mnt/resources/404.html
fi

# Start server
static-web-server \
    -p 3000 \
    --log-level info \
    --basic-auth="$AUTH" \
    --page404="/mnt/resources/404.html" \
    --page-fallback="$PAGE_FALLBACK" \
    --ignore-hidden-files false \
    --disable-symlinks \
    --compression false \
    --cache-control-headers false \
    --config-file /usr/local/server/helpers/config.toml \
    -d /usr/local/server/src/function
