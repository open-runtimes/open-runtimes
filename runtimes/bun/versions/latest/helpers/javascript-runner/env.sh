#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/javascript/helpers/<framework>/server.sh.
#
# OPR_JAVASCRIPT_RUNNER  — the binary the shared script uses to launch SSR entries
#                          with the preload that installs the Open Runtimes
#                          contract via src/ssr/injections.ts

export OPR_JAVASCRIPT_RUNNER="bun --preload /usr/local/server/src/ssr/injections.ts"
