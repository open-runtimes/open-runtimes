set -e

cd /usr/local/server/src/function/

source /usr/local/server/helpers/astro/env.sh

cp ../server-astro.js ./server.js
cp ../helpers.js ./helpers.js
cp ../logger.js ./logger.js

HOST=0.0.0.0 PORT=3000 node ./server.js
