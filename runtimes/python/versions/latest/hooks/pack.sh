#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Editable installs (pip install -e ./packages/x) point at the build directory,
# but the code runs from /usr/local/server/src/function
runtime_site=$(echo /usr/local/build/runtime-env/lib/python*/site-packages)
find "$runtime_site" -maxdepth 1 \( -name '*.pth' -o -name '__editable__*.py' \) -exec sed -i \
	-e 's#/usr/local/build/#/usr/local/server/src/function/#g' \
	-e 's#^/usr/local/build$#/usr/local/server/src/function#' {} +
