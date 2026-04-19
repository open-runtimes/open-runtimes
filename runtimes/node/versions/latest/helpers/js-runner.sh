#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/shared/js/helpers/<framework>/server.sh.
#
# OPR_JS_RUNNER      — the binary the shared script uses to launch SSR entries
# NODE_OPTIONS       — ensures user-owned http.createServer flows inherit the
#                      Open Runtimes contract via src/ssr/injections.mjs

export OPR_JS_RUNNER="node"
export NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"'
