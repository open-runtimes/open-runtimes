#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export ASTRO_TELEMETRY_DISABLED="1"
export ASTRO_NODE_AUTOSTART="enabled"