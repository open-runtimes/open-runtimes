#!/bin/sh
cd /usr/workspace
tar -zxf code.tar.gz
rm code.tar.gz
./Run serve --env production --hostname 0.0.0.0 --port 3000