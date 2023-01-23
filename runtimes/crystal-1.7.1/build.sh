#!/usr/bin/env sh

# Fail build if any command fails
set -e

echo "running build.sh"

# Go to user code directory
cd /usr/code

# Install dependencies
shards install --without-development

# Move back to the server directory
cd /usr/local/src

# Replace {entrypoint} in server.cr with INTERNAL_RUNTIME_ENTRYPOINT
sed -i "s#{entrypoint}#$INTERNAL_RUNTIME_ENTRYPOINT#g" server.cr

# Install dependencies
shards install --without-development

# Copy user code to the lib directory
mkdir -p /usr/local/src/lib/runtime_user_code
cp -R /usr/code/* /usr/local/src/lib/runtime_user_code

# Compile the code
shards build --production

# Finish build by preparing tar to use for starting the runtime
cd bin
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .