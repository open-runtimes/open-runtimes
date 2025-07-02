#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

bun src/server.ts
