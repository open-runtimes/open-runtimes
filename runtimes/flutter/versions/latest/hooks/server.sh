#!/bin/bash
set -e

# Dependencies are installed at image build time.
cd /usr/local/server/static-server
exec dart run server.dart
