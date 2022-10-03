#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz
cd /usr/local/src
cp -R /usr/code-start/local local

if [ -f "/usr/code-start/cpanfile" ]; then
   cat /usr/code-start/cpanfile >> /usr/local/src/cpanfile
fi

export PERL5LIB=./lib:./local/lib/perl5:$PERL5LIB

echo $PERL5LIB
carton install
perl server.pl daemon -l http://*:3000
