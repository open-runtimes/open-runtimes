#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Merge the user's dependencies into a copy of the server Gemfile in /tmp, so
# start-up writes nothing into the image or the code
mkdir -p /tmp/opr-bundle
cp /usr/local/server/Gemfile /usr/local/server/Gemfile.lock /tmp/opr-bundle/
if [ -f "/usr/local/server/src/function/Gemfile" ]; then
	echo "eval_gemfile '/usr/local/server/src/function/Gemfile'" >>/tmp/opr-bundle/Gemfile
fi
export BUNDLE_GEMFILE=/tmp/opr-bundle/Gemfile
export BUNDLE_PATH=/usr/local/server/src/function/vendor/bundle
