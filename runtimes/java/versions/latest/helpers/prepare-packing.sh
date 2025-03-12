#!/bin/bash

mkdir -p /tmp/compiled
mv /usr/local/build/compiled/{.*,*} /tmp/compiled
rm -rf /usr/local/build
mkdir -p /usr/local/build
mv /tmp/compiled/{.*,*} /usr/local/build/
