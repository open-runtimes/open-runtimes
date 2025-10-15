#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export NUXT_TELEMETRY_DISABLED="1"

# Middleware-style
export NITRO_PRESET="node"

# Standalone-style
# export NITRO_PRESET="node_server"