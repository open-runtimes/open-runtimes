#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Create virtual env
python3 -m venv runtime-env
source runtime-env/bin/activate
