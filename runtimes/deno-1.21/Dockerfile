FROM denoland/deno:alpine-1.21.3

LABEL maintainer="team@appwrite.io"

ENV INTERNAL_RUNTIME_ENTRYPOINT=mod.ts

RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/code-start
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds

WORKDIR /usr/local/src

ENV DENO_DIR=/usr/builds/deno-cache

RUN deno install -qAf --unstable https://deno.land/x/denon@2.5.0/denon.ts

COPY . .

RUN chmod +x /usr/local/src/start.sh
RUN chmod +x /usr/local/src/build.sh

EXPOSE 3000

CMD [ "/usr/local/src/start.sh" ]