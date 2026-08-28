#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# The build output is archived and restored in a fresh runtime container.
# Copies keep the interpreter inside the artifact instead of leaving an
# absolute link to the build container's /usr/local/bin/python3.
python3 -m venv --copies runtime-env
source runtime-env/bin/activate
