#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cp -R /usr/code-start/node_modules/* /usr/local/src/node_modules
cd /usr/local/src
bun start
