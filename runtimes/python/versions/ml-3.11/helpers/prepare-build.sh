#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Create virtual env
python3 -m venv runtime-env
. runtime-env/bin/activate # OVERRIDE: Cant use source here
