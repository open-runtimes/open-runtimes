ENV OPEN_RUNTIMES_ENTRYPOINT=mod.ts
ENV DENO_DIR=/usr/builds/deno-cache
RUN deno install -qAf --unstable https://deno.land/x/denon@2.5.0/denon.ts
