#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cd /usr/local/src
cp -R /usr/code-start/local .

export PERL5LIB=./lib:./local/lib/perl5:$PERL5LIB
perl server.pl daemon -m production -l http://*:3000 
