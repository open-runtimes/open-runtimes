RUN apk update && apk add bash nss font-noto ca-certificates

ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache

# Pre-populate /usr/local/server/node_modules so the shared SSR preload's
# `import superjson from "superjson"` resolves under --node-modules-dir=manual
# at runtime. Deno's alpine image has no npm, so we use deno install against
# the runtimes/javascript/src/ssr/deno.json import map that's already been
# overlaid into /usr/local/server/src/ssr/.
#
# Only deno 2.x exposes the npm-style `deno install` (with --allow-scripts and
# --node-modules-dir=auto|manual). On deno 1.x the same subcommand is the
# CLI-script installer where --node-modules-dir takes only true|false and
# --allow-scripts doesn't exist, so we skip the step there. Deno 1.x also
# predates --import (required for the SSR preload) so SSR isn't viable on
# those versions anyway. Probing --allow-scripts is a clean 2.x-only signal.
RUN if [ -f /usr/local/server/src/ssr/deno.json ] && \
	deno install --help 2>/dev/null | grep -q -- '--allow-scripts'; then \
	cp /usr/local/server/src/ssr/deno.json /usr/local/server/deno.json && \
	cd /usr/local/server && deno install --node-modules-dir=auto --allow-scripts; \
	fi
