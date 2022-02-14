#!/bin/sh

cd /usr/local/src/
bundle config set --local path 'vendor/bundle'
bundle install

cd /usr/code
if [[ -f "Gemfile" ]]; then
    bundle config set --local path '/usr/local/src/vendor/bundle'
    bundle install
fi

cp -r /usr/local/src/vendor /usr/code/vendor