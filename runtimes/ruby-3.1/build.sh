#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Append User Function Dependencies if Gemfile exists
cd /usr/builds

if [[ ! -f "Gemfile" ]]; then
    mv /usr/local/src/Gemfile.fallback /usr/builds/Gemfile
fi

EXISTING_GEMFILE=$(cat /usr/local/src/Gemfile)
echo "eval_gemfile '/usr/builds/Gemfile'" >> /usr/local/src/Gemfile

# Prepare dependencies
cd /usr/local/src/
bundle config set --local path 'vendor/bundle'

INSTALL_COMMAND=${1:-'bundle install'}
BUILD_COMMAND=${2:-''}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

echo "${EXISTING_GEMFILE}" > Gemfile

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
cp -R /usr/local/src/vendor .
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .