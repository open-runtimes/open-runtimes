#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export NEXT_TELEMETRY_DISABLED="1"