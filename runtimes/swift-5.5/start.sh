#!/bin/sh

cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code 
rm /usr/workspace/code.tar.gz
cd /usr/local/src

# TODO: Set architecture to x86_64 instead of aarch64 when appropriate
.build/aarch64-unknown-linux-gnu/release/Run serve --env production --hostname 0.0.0.0 --port 3000