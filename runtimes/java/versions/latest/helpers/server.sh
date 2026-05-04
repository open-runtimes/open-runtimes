#!/bin/bash
set -e
shopt -s dotglob

if [ -f src/function/server ]; then
    ./src/function/server
else
    java -jar src/function/java-runtime-1.0.0.jar
fi
