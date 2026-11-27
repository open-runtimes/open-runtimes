#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# A NestJS app is an Express (or Fastify) server built to dist/ by its own build
# command, so it starts the way an Express app does: the entrypoint (dist/main.js)
# runs with PORT and HOST set, and the server listening on PORT drains on SIGTERM.
exec bash /usr/local/server/helpers/express/server.sh
