set -e

cd /usr/local/server/src/function/

source /usr/local/server/helpers/sveltekit/env.sh

cp ../server-sveltekit.js ./server.js
mkdir -p ./ssr
cp -R ../ssr/* ./ssr/

HOST=0.0.0.0 PORT=3000 node ./server.js