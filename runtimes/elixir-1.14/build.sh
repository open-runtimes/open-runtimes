#!/bin/sh

set -e

cp -a /usr/code/. /usr/local/src/server/lib/usr/

cd /usr/local/src/server

# Get local.hex and local.rebar for building app
mix local.hex --force
mix local.rebar --force

# Build elixir app production release
MIX_ENV=prod mix release

# Tar the app
tar -C /usr/local/src/server/_build/prod/rel/server -czvf /usr/code/code.tar.gz .
