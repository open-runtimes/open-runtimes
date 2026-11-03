#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Create virtual env
python3 -m venv runtime-env
. runtime-env/bin/activate # OVERRIDE: Cant use source here

# Expose the server-env packages to the runtime-env interpreter via a .pth
# file, written here so that start-up writes nothing into the code
server_site=$(echo /usr/local/server/server-env/lib/python*/site-packages)
echo "$server_site" >"$(echo runtime-env/lib/python*/site-packages)/server-env.pth"
