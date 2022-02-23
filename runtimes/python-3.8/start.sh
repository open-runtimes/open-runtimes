#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
source /usr/code-start/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/code-start/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
cd /usr/local/src/
python3 server.py