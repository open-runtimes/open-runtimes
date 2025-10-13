#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Parse float-like string to integer with trick to round-up
cpu_cores=$(echo "$OPEN_RUNTIMES_CPUS" | awk '{print int($1 + 0.999999)}')

workers=$((1 * $cpu_cores))

# Ensure at least 1 worker
if [ "$workers" -eq 0 ]; then
    workers=1
fi

echo "HTTP server successfully started!"

python3 /usr/local/server/server-env/bin/gunicorn \
  -b 0.0.0.0:3000 \
  --log-level='warning' \
  -w $workers \
  --chdir "$(pwd)/src" \
  --worker-class aiohttp.GunicornWebWorker \
  --preload \
  --timeout 0 \
  'server:app'