set -e

cd /usr/local/server/src/function/

mv ../server-analog.js ./server.js
mv ../helpers.js ./helpers.js
mv ../logger.js ./logger.js

HOST=0.0.0.0 PORT=3000 node ./server.js
