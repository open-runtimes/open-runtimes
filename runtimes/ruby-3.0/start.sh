#!/bin/sh
cp /tmp/code.tar.gz /usr/workspace/code.tar.gz 
cd /usr/workspace
tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start
rm /usr/workspace/code.tar.gz

set -o allexport
source /usr/code-start/.open-runtimes
set +o allexport

cd /usr/local/src
cp -R /usr/code-start/vendor .

if [ -f "/usr/code-start/Gemfile" ]; then
    echo "eval_gemfile '/usr/code-start/Gemfile'" >> /usr/local/src/Gemfile
fi

bundle config set --local path 'vendor/bundle'
bundle exec puma -b tcp://0.0.0.0:3000 -e production