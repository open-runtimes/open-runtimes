#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace 
tar -zxf /usr/workspace/code.tar.gz -C /usr/code 
rm /usr/workspace/code.tar.gz
source /usr/code/runtime-env/bin/activate
cd /usr/local/src/
flask run --host=0.0.0.0 --port=3000