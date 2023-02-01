#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Install User Function Dependencies if package.json exists
cd /usr/builds
if [[ ! -f "composer.json" ]]; then
    mv /usr/local/src/composer.json.fallback /usr/builds/composer.json
fi

php /usr/local/src/prepare.php

cd /usr/local/src/

INSTALL_COMMAND=${1:-'composer install --no-interaction --ignore-platform-reqs --optimize-autoloader --no-scripts --prefer-dist --no-dev'}
BUILD_COMMAND=${2:-''}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

cp -r /usr/local/src/vendor /usr/builds/vendor

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .
