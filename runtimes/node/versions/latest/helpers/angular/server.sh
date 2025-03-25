#!/bin/bash
set -e
shopt -s dotglob

cd /usr/local/server/src/function/

source /usr/local/server/helpers/angular/env.sh

cp ../server-angular.mjs ./server.mjs
mkdir -p ./ssr
cp -R ../ssr/* ./ssr/

HOST=0.0.0.0 PORT=3000 node ./server.mjs
