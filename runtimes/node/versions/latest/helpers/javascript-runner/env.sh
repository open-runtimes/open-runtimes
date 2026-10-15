#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/javascript/helpers/<framework>/server.sh.
#
# OPR_JAVASCRIPT_RUNNER      — the binary the shared script uses to launch SSR entries
# NODE_OPTIONS       — ensures user-owned http.createServer flows inherit the
#                      Open Runtimes contract via src/ssr/injections.mjs

# Left to itself, V8 derives its old-space ceiling from the cgroup limit and
# lands at roughly half of it, so an SSR site can only reach ~1 GB of heap on a
# 2 GB allocation before dying with "Ineffective mark-compacts near heap limit"
# while half its memory sits unused. Size the ceiling from the allocation
# instead, holding 20% back for the non-heap side (code, buffers, native
# allocations) so V8 hits its own GC ceiling first — the kernel OOM killer
# would take the process down with no JS stack trace to debug from.
#
# Functions get this via --max_old_space_size in OPEN_RUNTIMES_SERVER_COMMAND
# (runtimes/node/Dockerfile); SSR entries never go through that command.
heap_mb="$(echo "$OPEN_RUNTIMES_MEMORY" | awk '{ mb = int($1 * 0.8) } END { print (mb > 0 ? mb : 1638) }')"

export OPR_JAVASCRIPT_RUNNER="node --max_old_space_size=$heap_mb"
export NODE_OPTIONS='--import "/usr/local/server/src/ssr/injections.mjs"'
