#!/bin/bash
# Per-runtime knobs consumed by the shared SSR server.sh in
# runtimes/javascript/helpers/<framework>/server.sh.
#
# OPR_JAVASCRIPT_RUNNER  — the binary (plus flags) the shared script uses to
#                          launch SSR entries. Deno needs:
#                            --allow-all
#                                (user framework servers touch fs/net/env/run)
#                            --node-modules-dir=manual
#                                (use the node_modules folder bundled by
#                                 bundle.sh from the user's npm install — no
#                                 re-download, no deno-managed folder. Bare
#                                 specifiers like `import express from
#                                 "express"` resolve like they do under
#                                 node/bun.)
#                            --config=.../src/ssr/deno.json
#                                (an import map that makes the bare specifiers
#                                 used by the shared preload — currently just
#                                 "superjson" — resolvable to npm: packages.
#                                 Node/bun don't read this file; they resolve
#                                 those same imports from
#                                 /usr/local/server/node_modules at image
#                                 build time via npm/bun install.)
#                            --import=.../src/ssr/injections.ts
#                                (load the preload module that patches http
#                                 before framework code runs)

export OPR_JAVASCRIPT_RUNNER="deno run --allow-all --node-modules-dir=manual --config=/usr/local/server/src/ssr/deno.json --import=/usr/local/server/src/ssr/injections.ts"
