RUN apk update && apk add bash nss font-noto ca-certificates

ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache

# Pre-populate /usr/local/server/node_modules so the shared SSR preload's
# `import superjson from "superjson"` resolves under --node-modules-dir=manual
# at runtime. Deno's alpine image has no npm, so we use deno install against
# the runtimes/javascript/src/ssr/deno.json import map that's already been
# overlaid into /usr/local/server/src/ssr/.
#
# Only deno 2.x exposes the npm-style `deno install --node-modules-dir`. On
# deno 1.x `deno install` is the CLI-script installer with a different flag
# set, so we skip the step there — deno 1.x also predates the preload
# mechanism (--import) the SSR runtime relies on, so SSR isn't viable on
# those versions anyway.
RUN if [ -f /usr/local/server/src/ssr/deno.json ] && \
	deno install --help 2>/dev/null | grep -q -- '--node-modules-dir'; then \
	cp /usr/local/server/src/ssr/deno.json /usr/local/server/deno.json && \
	cd /usr/local/server && deno install --node-modules-dir=auto --allow-scripts; \
	fi
