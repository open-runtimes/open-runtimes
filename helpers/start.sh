#!/bin/bash
# Executor contract: helpers/start.sh "<start command>"
exec tini -s -g -- bash /usr/local/server/helpers/lifecycle/start.sh "$@"
