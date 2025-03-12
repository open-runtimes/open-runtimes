#!/bin/sh

mkdir -p /tmp/compiled
cp -R /usr/local/build/compiled/. /tmp/compiled
rm -rf /usr/local/build
mkdir -p /usr/local/build
cp -R /tmp/compiled/. /usr/local/build/
