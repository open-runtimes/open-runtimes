set -e

cd /usr/local/server/src/function/

source /usr/local/server/helpers/nuxt/env.sh

cp ../server-nuxt.mjs ./server.mjs
mkdir -p ./ssr
cp -R ../ssr/. ./ssr/

HOST=0.0.0.0 PORT=3000 node ./server.mjs