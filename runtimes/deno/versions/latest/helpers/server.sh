#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

deno run --allow-run --allow-net --allow-write --allow-read --allow-env src/server.ts
