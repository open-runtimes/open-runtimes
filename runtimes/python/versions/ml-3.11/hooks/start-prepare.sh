#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Builds made before build-prepare.sh wrote server-env.pth still need it. A
# runtime that cannot write it (non-root) appends server-env from a
# sitecustomize in /tmp instead, keeping it after runtime-env as the .pth does.
runtime_site=$(echo /usr/local/server/src/function/runtime-env/lib/python*/site-packages)
if [ ! -f "$runtime_site/server-env.pth" ]; then
	server_site=$(echo /usr/local/server/server-env/lib/python*/site-packages)
	if [ -w "$runtime_site" ]; then
		echo "$server_site" >"$runtime_site/server-env.pth"
	else
		mkdir -p /tmp/opr-site
		echo "import site; site.addsitedir('$server_site')" >/tmp/opr-site/sitecustomize.py
		export PYTHONPATH="/tmp/opr-site${PYTHONPATH:+:$PYTHONPATH}"
	fi
fi

# Activate virtual env
. /usr/local/server/src/function/runtime-env/bin/activate # OVERRIDE: Cant use source here
export VIRTUAL_ENV="/usr/local/server/src/function/runtime-env"
export PATH="$VIRTUAL_ENV/bin:$PATH"
