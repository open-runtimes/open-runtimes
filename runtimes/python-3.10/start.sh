#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code 
rm /usr/workspace/code.tar.gz
source /usr/code/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/code/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
cd /usr/local/src/
python3 /usr/code/runtime-env/bin/flask run --host=0.0.0.0 --port=3000