#!/bin/sh
echo "OK1"
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
echo "OK2"
cd /usr/workspace/
echo "OK3"
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
echo "OK4"
rm /usr/workspace/code.tar.gz
echo "OK5"
cp -r /usr/code-start/vendor /usr/local/src/vendor
echo "OK6"
cd /usr/local/src/
php server.php