#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Hardlink the server-env virtual env into the runtime-env virtual env
# (same filesystem, so -l makes this near-instant instead of a full copy).
# Pre-extracted deployments can resolve runtime-env onto a separate mount,
# where hardlinks fail with EXDEV — fall back to a plain copy there.
cp -rl /usr/local/server/server-env/* /usr/local/server/src/function/runtime-env 2>/dev/null ||
	cp -r /usr/local/server/server-env/* /usr/local/server/src/function/runtime-env

# Activate virtual env
. /usr/local/server/src/function/runtime-env/bin/activate # OVERRIDE: Cant use source here
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
