#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Middleware-style & Standalone-style (together)
export GCP_BUILDPACKS="open-runtimes" # Ensures "node" adapter in adapter-auto
