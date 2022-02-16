#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace 
tar -zxf /usr/workspace/code.tar.gz -C /usr/code 
rm /usr/workspace/code.tar.gz
cd /usr/code
/usr/code/runtime