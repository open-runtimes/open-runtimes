#!/bin/sh

cp /tmp/code.tar.gz /usr/workspace/code.tar.gz
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code
rm /usr/workspace/code.tar.gz

set -o allexport
source /usr/code-start/.open-runtimes
set +o allexport

cd /usr/code
java -jar java-runtime-1.0.0.jar