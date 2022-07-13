#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace 
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cd /usr/local/src/

# Prepare dependencies
cp -R /usr/code-start/vendor .
mkdir -p vendor

# Run script, using import map if we have one
FILE=vendor/import_map.json
if [ -f "$FILE" ]; then
    denon run --import-map=vendor/import_map.json --no-check --allow-net --allow-write --allow-read --allow-env server.ts
else 
    denon run --no-check --allow-net --allow-write --allow-read --allow-env server.ts
fi