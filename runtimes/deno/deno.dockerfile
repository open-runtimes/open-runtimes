RUN apk update && apk add bash nss font-noto ca-certificates

ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache

# Pre-populate /usr/local/server/node_modules so the shared SSR preload's
# `import superjson from "superjson"` resolves under --node-modules-dir=manual
# at runtime. Deno's alpine image has no npm, so we use deno install against
# the runtimes/javascript/src/ssr/deno.json import map that's already been
# overlaid into /usr/local/server/src/ssr/.
RUN if [ -f /usr/local/server/src/ssr/deno.json ]; then \
	cp /usr/local/server/src/ssr/deno.json /usr/local/server/deno.json && \
	cd /usr/local/server && deno install --node-modules-dir=auto --allow-scripts; \
	fi
