#!/bin/bash
set -e
shopt -s dotglob

node --max_old_space_size=8192 src/server.js