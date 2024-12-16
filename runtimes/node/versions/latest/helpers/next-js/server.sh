set -e

cd /usr/local/server/src/function/

HOST=0.0.0.0 PORT=3000 npm start # TODO: Security for port 3000
