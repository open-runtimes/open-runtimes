set -e

cd /usr/local/server/src/function/

# To be used with proxy.sh
HOST=127.0.0.1 PORT=3001 node server/entry.mjs
