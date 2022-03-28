#!/bin/sh
cp /tmp/code.tar.gz /usr/code-start/code.tar.gz
cd /usr/code-start
tar -xzf /usr/code-start/code.tar.gz
rm /usr/code-start/code.tar.gz
dotnet DotNetRuntime.dll
