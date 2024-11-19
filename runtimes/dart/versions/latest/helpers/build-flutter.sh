#!/bin/sh
# Fail build if any command fails
set -e

. /usr/local/server/helpers/before-build.sh

# Flutter CLI
git clone --depth 1 --branch 3.24.4 https://github.com/flutter/flutter.git /tmp/flutter
export PATH="/tmp/flutter/bin:/tmp/flutter/bin/cache/dart-sdk/bin:$PATH"

# Flutter Web SDK
flutter config --enable-web

# Template build
git clone https://github.com/Meldiron/flutter-web-test.git /tmp/template
cd /tmp/template
flutter build web
ls -al build/web

# Custom commands
sh -c "$1"

. /usr/local/server/helpers/after-build.sh
