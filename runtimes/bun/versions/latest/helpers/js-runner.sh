#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/shared/js/helpers/<framework>/server.sh.
#
# OPR_JS_RUNNER  — the binary the shared script uses to launch SSR entries
# BUN_OPTIONS    — ensures user-owned http.createServer flows inherit the
#                  Open Runtimes contract via src/ssr/injections.ts

export OPR_JS_RUNNER="bun"
export BUN_OPTIONS="--preload /usr/local/server/src/ssr/injections.ts"
