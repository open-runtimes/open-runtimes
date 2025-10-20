#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Middleware-style
# export NITRO_PRESET="node"

# Standalone-style
export NITRO_PRESET="node_server"