set -e

# https://docs.gunicorn.org/en/latest/design.html#how-many-workers
cpu_cores=$(nproc)
workers=$((1 + 2 * cpu_cores))

echo "HTTP server successfully started!"
python3 /usr/local/server/src/function/runtime-env/bin/gunicorn -b 0.0.0.0:3000 --log-level='warning' -w $workers --chdir "$(pwd)/src" 'server:app'