#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

export GCP_BUILDPACKS="open-runtimes"