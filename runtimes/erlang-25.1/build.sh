#!/bin/sh

set -e

cp -a /usr/code/. /usr/local/src/server/src/

cd /usr/local/src/server

# Build production release of erlang app
rebar3 as prod release

# Tar the app
tar -zcvf /usr/code/code.tar.gz /usr/local/src/server/_build/prod/rel/server/
