set -e

cd /usr/local/server/src/function/

mv ../server-astro.js ./server.js

HOST=0.0.0.0 PORT=3000 node ./server.js
