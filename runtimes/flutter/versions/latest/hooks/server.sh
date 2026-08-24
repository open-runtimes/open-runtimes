#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# dhttpd is preinstalled at image build time
export PATH="$PATH":"$HOME/.pub-cache/bin"

# Run HTTP server
# TODO: Port 3000 should be protected if actually used outside of just tests
echo "HTTP server successfully started!"
dhttpd --host 0.0.0.0 --port 3000 --path /usr/local/server/src/function/
