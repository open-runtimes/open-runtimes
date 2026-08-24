#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Link user's depenrencies
if [ -f "/usr/local/server/src/function/Gemfile" ]; then
	echo "eval_gemfile '/usr/local/server/src/function/Gemfile'" >>/usr/local/server/Gemfile
fi

# Link dependencies (symlink instead of copy — the bundle can be tens of MB)
ln -sfn /usr/local/server/src/function/vendor /usr/local/server/vendor

bundle config set --local path 'vendor/bundle'
