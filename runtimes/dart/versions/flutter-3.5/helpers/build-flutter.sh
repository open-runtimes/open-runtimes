#!/bin/sh
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-build.sh

# Flutter CLI
git clone --depth 1 --branch 3.24.4 https://github.com/flutter/flutter.git /tmp/flutter
chmod -R 777 /tmp/flutter/bin/cache/dart-sdk/bin
export PATH="/tmp/flutter/bin:/tmp/flutter/bin/cache/dart-sdk/bin:$PATH"

# Flutter Web SDK
flutter config --enable-web

flutter doctor

# Custom commands
sh -c "$1"

. /usr/local/server/helpers/after-build.sh
