#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export ASTRO_TELEMETRY_DISABLED="1"

# Middleware-style
export ASTRO_NODE_AUTOSTART="disabled"

# Standalone-style
# export ASTRO_NODE_AUTOSTART="enabled"
