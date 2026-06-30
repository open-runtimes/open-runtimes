#!/bin/bash
set -e
shopt -s dotglob

# Write files to serve system routes
mkdir -p /usr/local/server/src/function/__opr
echo -n "OK" >/usr/local/server/src/function/__opr/health.txt
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

# Generate dynamic config.toml with HEADER_NAME support
if [ -z "$OPEN_RUNTIMES_CACHE_HEADER" ]; then
	OPEN_RUNTIMES_CACHE_HEADER="CDN-Cache-Control"
fi

# Echo $1 if it's a non-negative integer, else the fallback $2 (keeps the TOML valid).
sanitize_int() {
	case "$1" in
	'' | *[!0-9]*) echo "$2" ;;
	*) echo "$1" ;;
	esac
}

# Follow symlinks in the served dir. Off by default (path-escape defense);
# enable when code is mounted as symlinks (e.g. sidecar), else those paths 404.
OPEN_RUNTIMES_STATIC_FOLLOW_SYMLINKS="$(echo "${OPEN_RUNTIMES_STATIC_FOLLOW_SYMLINKS:-false}" | tr '[:upper:]' '[:lower:]')"
if [ "$OPEN_RUNTIMES_STATIC_FOLLOW_SYMLINKS" = "true" ]; then
	DISABLE_SYMLINKS="false"
else
	DISABLE_SYMLINKS="true"
fi

# Toggle the in-memory cache.
OPEN_RUNTIMES_STATIC_MEMORY_CACHE_ENABLED="$(echo "${OPEN_RUNTIMES_STATIC_MEMORY_CACHE_ENABLED:-true}" | tr '[:upper:]' '[:lower:]')"

# Time-to-live in seconds.
OPEN_RUNTIMES_STATIC_CACHE_TTL="$(sanitize_int "${OPEN_RUNTIMES_STATIC_CACHE_TTL:-86400}" 86400)"

# Time-to-idle in seconds; defaults to TTL.
OPEN_RUNTIMES_STATIC_CACHE_TTI="$(sanitize_int "${OPEN_RUNTIMES_STATIC_CACHE_TTI:-$OPEN_RUNTIMES_STATIC_CACHE_TTL}" "$OPEN_RUNTIMES_STATIC_CACHE_TTL")"

# Max cached files.
OPEN_RUNTIMES_STATIC_CACHE_CAPACITY="$(sanitize_int "${OPEN_RUNTIMES_STATIC_CACHE_CAPACITY:-4096}" 4096)"

# Max cacheable file size (KiB).
OPEN_RUNTIMES_STATIC_CACHE_MAX_FILE_KIB="$(sanitize_int "${OPEN_RUNTIMES_STATIC_CACHE_MAX_FILE_KIB:-8192}" 8192)"

# Create dynamic config.toml
cat >/tmp/config.toml <<EOF
[advanced]
EOF

# Append the in-memory cache table unless the feature is disabled.
if [ "$OPEN_RUNTIMES_STATIC_MEMORY_CACHE_ENABLED" = "true" ]; then
	cat >>/tmp/config.toml <<EOF

[advanced.memory-cache]
capacity = $OPEN_RUNTIMES_STATIC_CACHE_CAPACITY
ttl = $OPEN_RUNTIMES_STATIC_CACHE_TTL
tti = $OPEN_RUNTIMES_STATIC_CACHE_TTI
max-file-size = $OPEN_RUNTIMES_STATIC_CACHE_MAX_FILE_KIB
EOF
fi

cat >>/tmp/config.toml <<EOF

[[advanced.rewrites]]
source = "/__opr/timings"
destination = "/__opr/timings.txt"

[[advanced.rewrites]]
source = "/__opr/health"
destination = "/__opr/health.txt"

[[advanced.rewrites]]
source = "**/.env*"
destination = "/tmp/__opr/empty"

[[advanced.headers]]
source = "**/*"
[advanced.headers.headers]
$OPEN_RUNTIMES_CACHE_HEADER = "public, max-age=36000"
EOF

# Start server
echo "HTTP server successfully started!"
static-web-server \
	-p 3000 \
	--log-level info \
	--basic-auth="$AUTH" \
	--page404="/mnt/resources/404.html" \
	--page-fallback="$PAGE_FALLBACK" \
	--ignore-hidden-files false \
	--disable-symlinks "$DISABLE_SYMLINKS" \
	--compression false \
	--cache-control-headers false \
	--config-file /tmp/config.toml \
	-d /usr/local/server/src/function
