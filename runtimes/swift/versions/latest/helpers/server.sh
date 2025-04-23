#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

src/function/Runtime serve --env production --hostname 0.0.0.0 --port 3000