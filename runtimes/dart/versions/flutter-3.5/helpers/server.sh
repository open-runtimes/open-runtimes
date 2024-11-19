#!/bin/sh
# Fail build if any command fails
set -e

# Install Dart static server
dart pub global activate dhttpd

# Run HTTP server
# TODO: Port 3000 should be protected if actually used outside of just tests
dhttpd --host 0.0.0.0 --port 3000 --path /usr/local/server/src/function/