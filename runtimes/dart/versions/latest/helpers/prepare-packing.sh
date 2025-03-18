#!/bin/sh

mkdir -p /tmp/compiled
mv $(ls -A /usr/local/build/compiled/* | awk '{print "/usr/local/build/compiled/*" $0}') /tmp/compiled
rm -rf /usr/local/build && mkdir -p /usr/local/build
mv $(ls -A /tmp/compiled/* | awk '{print "/tmp/compiled/*" $0}') ./usr/local/build
