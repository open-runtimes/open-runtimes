set -e

# 1 worker for each CPU core assigned
cpu_cores=$(echo "$OPEN_RUNTIMES_CPUS" | awk '{print int($1 + 0.999999)}') # Parse float-like string to integer with trick to round-up
workers=$((1 + (2 * $cpu_cores)))

echo "HTTP server successfully started!"
python3 /usr/local/server/src/function/runtime-env/bin/gunicorn -b 0.0.0.0:3000 --log-level='warning' -w $workers --chdir "$(pwd)/src" --worker-class aiohttp.GunicornWebWorker 'server:app'
