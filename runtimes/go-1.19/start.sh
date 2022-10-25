#!bin/sh 

cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace 
tar -zxf code.tar.gz -C /usr/local/src/server 
rm /usr/workspace/code.tar.gz 
cd /usr/local/src/server 
go run *.go 