#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/javascript/helpers/<framework>/server.sh.
#
# OPR_JAVASCRIPT_RUNNER  — the binary (plus flags) the shared script uses to
#                          launch SSR entries. Deno needs:
#                            --allow-all             (user framework servers
#                                                     touch fs/net/env/run)
#                            --node-modules-dir=auto (resolve bare specifiers
#                                                     like `import express
#                                                     from "express"` out of
#                                                     the user's node_modules
#                                                     installed by `npm i`)
#                            --import=…injections.ts (load the preload module
#                                                     that patches http before
#                                                     framework code runs)

export OPR_JAVASCRIPT_RUNNER="deno run --allow-all --node-modules-dir=auto --import=/usr/local/server/src/ssr/injections.ts"
