#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Install User Function Dependencies if requirements.txt exists
cd /usr/builds
python3 -m venv runtime-env
source runtime-env/bin/activate

if [[ ! -f "requirements.txt" ]]; then
    mv /usr/local/src/requirements.txt.fallback /usr/builds/requirements.txt
fi

INSTALL_COMMAND=${1:-'pip install --no-cache-dir -r requirements.txt'}
BUILD_COMMAND=${2:-''}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

# Merge the requirements from server and user code
cd /usr/local/src/
pip install --no-cache-dir -r requirements.txt

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .