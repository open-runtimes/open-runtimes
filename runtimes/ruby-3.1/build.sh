#!/bin/sh

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Append User Function Dependencies if Gemfile exists
cd /usr/builds
if [[ -f "Gemfile" ]]; then
    echo "eval_gemfile '/usr/builds/Gemfile'" >> /usr/local/src/Gemfile
fi

# Prepare server dependencies
cd /usr/local/src/
bundle config set --local path 'vendor/bundle'
bundle install

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
cp -R /usr/local/src/vendor /usr/builds/vendor
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .