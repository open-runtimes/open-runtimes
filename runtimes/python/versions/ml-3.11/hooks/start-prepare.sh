#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Builds made before build-prepare.sh wrote server-env.pth still need it
runtime_site=$(echo /usr/local/server/src/function/runtime-env/lib/python*/site-packages)
if [ ! -f "$runtime_site/server-env.pth" ]; then
	echo /usr/local/server/server-env/lib/python*/site-packages >"$runtime_site/server-env.pth"
fi

# Activate virtual env
. /usr/local/server/src/function/runtime-env/bin/activate # OVERRIDE: Cant use source here
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
