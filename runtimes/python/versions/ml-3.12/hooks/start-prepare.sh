#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Hardlink the server-env virtual env into runtime-env; fall back to a
# copy when runtime-env resolves onto a different filesystem
cp -rl /usr/local/server/server-env/* /usr/local/server/src/function/runtime-env 2>/dev/null ||
	cp -r /usr/local/server/server-env/* /usr/local/server/src/function/runtime-env

# Activate virtual env
. /usr/local/server/src/function/runtime-env/bin/activate # OVERRIDE: Cant use source here
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
