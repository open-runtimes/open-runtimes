#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cp -R /usr/code-start/node_modules/* /usr/local/src/node_modules
cd /usr/local/src

close() {
  kill -s SIGINT $(pgrep -f "node /usr/local/src/server.js")
  sleep 3
  kill -TERM "$child" 2>/dev/null
}

trap close SIGTERM

npm start &

child=$!
wait "$child"