set -e

cd /usr/local/server/src/function/

source /usr/local/server/helpers/analog/env.sh

cp ../server-analog.mjs ./server.mjs
mkdir -p ./ssr
cp -R ../ssr/. ./ssr/

HOST=0.0.0.0 PORT=3000 node ./server.mjs
