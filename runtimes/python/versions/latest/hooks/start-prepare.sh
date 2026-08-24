#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Hardlink the server-env virtual env into the runtime-env virtual env
# (same filesystem, so -l makes this near-instant instead of a full copy)
cp -rl /usr/local/server/server-env/* /usr/local/server/src/function/runtime-env

# Activate virtual env
source /usr/local/server/src/function/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
