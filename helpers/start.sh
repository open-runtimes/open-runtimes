#!/bin/bash
# Executor contract: helpers/start.sh "<start command>"
exec tini -s -- bash /usr/local/server/helpers/lifecycle/supervise.sh bash /usr/local/server/helpers/lifecycle/start.sh "$@"
