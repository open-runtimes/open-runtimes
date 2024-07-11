#!/bin/bash
# Fail build if any command fails
set -e

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/.open-runtimes

# Append server's dependnecies
cd /usr/local/server
pip install --no-cache-dir -r requirements.txt

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."
