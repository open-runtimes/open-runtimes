#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

java -jar src/function/kotlin-runtime-1.0.0.jar
