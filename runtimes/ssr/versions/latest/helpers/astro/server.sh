set -e

echo "OK1"

cd /usr/local/server/src/function/

echo "OK2"

mv ../server-astro.js ./server.js

echo "OK3"

ls -al ../

pwd
ls -al .

HOST=0.0.0.0 PORT=3000 node ./server.js
