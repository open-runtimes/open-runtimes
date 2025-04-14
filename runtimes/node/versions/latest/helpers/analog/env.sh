#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export NITRO_PRESET="node"