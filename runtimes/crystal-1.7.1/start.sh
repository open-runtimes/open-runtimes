#!/usr/bin/env sh

echo "running start.sh"

cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cd /usr/code-start
/usr/code-start/server