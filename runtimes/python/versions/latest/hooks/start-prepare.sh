#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Expose the server-env packages to the runtime-env interpreter via a .pth
# file instead of copying the whole virtual env on every cold start
server_site=$(echo /usr/local/server/server-env/lib/python*/site-packages)
runtime_site=$(echo /usr/local/server/src/function/runtime-env/lib/python*/site-packages)
echo "$server_site" >"$runtime_site/server-env.pth"

# Activate virtual env
source /usr/local/server/src/function/runtime-env/bin/activate
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
