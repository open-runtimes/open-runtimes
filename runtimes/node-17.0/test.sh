#!/bin/sh

cp -R /usr/test-code/* /usr/code
sh /usr/local/src/build.sh
cd /usr/code
tar -zcvf /tmp/code.tar.gz .
cd ~
sh /usr/local/src/launch.sh