set -e

cd /usr/local/server/src/function/

HOST=0.0.0.0 PORT=3000 ./node_modules/.bin/remix-serve ./.build/server/index.js # TODO: Security for port 3000
