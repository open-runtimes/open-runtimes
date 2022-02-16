#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cd /usr/local/src
bundle config set --local path 'vendor/bundle'
bundle exec puma -b tcp://0.0.0.0:3000 -e production