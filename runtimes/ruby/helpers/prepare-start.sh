#!/bin/sh
# Fail build if any command fails
set -e

# Link user's depenrencies
if [ -f "/usr/local/server/src/function/Gemfile" ]; then
    echo "eval_gemfile '/usr/local/server/src/function/Gemfile'" >> /usr/local/server/Gemfile
fi

# Copy dependencies
cp -R /usr/local/server/src/function/vendor /usr/local/server/vendor

bundle config set --local path 'vendor/bundle'