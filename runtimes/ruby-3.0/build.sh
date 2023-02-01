#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Append User Function Dependencies if Gemfile exists
cd /usr/builds
if [ -f "Gemfile" ]; then
    EXISTING_GEMFILE=$(cat /usr/local/src/Gemfile)
    echo "eval_gemfile '/usr/builds/Gemfile'" >> /usr/local/src/Gemfile
fi

# Prepare dependencies
cd /usr/local/src/
bundle config set --local path 'vendor/bundle'
bundle install

if [ -f "Gemfile" ]; then
    echo "${EXISTING_GEMFILE}" > Gemfile
fi

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
cp -R /usr/local/src/vendor .

touch .open-runtimes
echo "INTERNAL_RUNTIME_ENTRYPOINT=$INTERNAL_RUNTIME_ENTRYPOINT" > .open-runtimes

tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .