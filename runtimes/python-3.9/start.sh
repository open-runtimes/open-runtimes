#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/local/src/
mkdir -p userlib
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/local/src/userlib
rm /usr/workspace/code.tar.gz
source /usr/local/src/userlib/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/local/src/userlib/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
cd /usr/local/src/
python3 server.py