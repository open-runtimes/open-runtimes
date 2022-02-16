#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/builds

# Prepare server dependencies
cd /usr/local/src/
bundle config set --local path 'vendor/bundle'
bundle install

# Install User Function Dependencies if Gemfile exists
cd /usr/builds
if [[ -f "Gemfile" ]]; then
    bundle config set --local path '/usr/local/src/vendor/bundle'
    bundle install
fi

# Merge the node_modules from the server into the user's node_modules to be restored later.
cp -r /usr/local/src/vendor /usr/builds/vendor

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .