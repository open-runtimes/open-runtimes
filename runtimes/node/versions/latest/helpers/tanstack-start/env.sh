#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Standalone-style, relevant for Nitro builds
export NITRO_PRESET="node_server"