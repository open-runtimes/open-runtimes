#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Dart static server is preinstalled at image build time (see runtimes/flutter/Dockerfile)
export PATH="$PATH":"$HOME/.pub-cache/bin"

# Run HTTP server
# TODO: Port 3000 should be protected if actually used outside of just tests
echo "HTTP server successfully started!"
dhttpd --host 0.0.0.0 --port 3000 --path /usr/local/server/src/function/
