set -e

cd /usr/local/server/src/function/

# To be used with server.sh (proxy)
HOST=127.0.0.1 PORT=3001 npm start
