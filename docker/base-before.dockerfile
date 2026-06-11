LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk add util-linux zstd squashfs-tools
    else
        apt-get update && apt-get install -y util-linux zstd squashfs-tools
    fi
EOR

RUN mkdir -p /mnt/code /mnt/logs /mnt/telemetry /usr/local/build /usr/local/server/src/function

WORKDIR /usr/local/server

# Assemble /usr/local/server from named build contexts, in overlay precedence
# order (later layers overwrite earlier ones):
#   1. helpers  -> repo helpers/ (global lifecycle runner + shims)
#   2. shared   -> runtimes/<shared family>/ (e.g. javascript/ for node), or empty
#   3. latest   -> runtimes/<runtime>/versions/latest/
#   4. version  -> runtimes/<runtime>/versions/<version>/ extras, or empty
COPY --from=helpers . /usr/local/server/helpers/
COPY --from=shared . /usr/local/server/
COPY --from=latest . /usr/local/server/
COPY --from=version . /usr/local/server/
RUN rm -f /usr/local/server/.gitkeep /usr/local/server/Dockerfile /usr/local/server/build.dockerfile