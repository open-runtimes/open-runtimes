LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"

ENV OPEN_RUNTIMES_SECRET=open_runtime_secret
ENV OPEN_RUNTIMES_ENV=production
ENV OPEN_RUNTIMES_HEADERS="{}"

# Disable registry audit requests while constructing runtime images. Keeping
# this as an ARG prevents the setting from leaking into the published image.
ARG npm_config_audit=false

# Upgrade OS packages first: many pinned bases (EOL runtime versions, frozen
# upstream images) are never rebuilt, so without this they keep shipping
# outdated musl/glibc, OpenSSL, ca-certificates, ... even when we rebuild.
# OS_UPGRADE=false skips only the upgrade, for bases whose distro repos are
# gone (e.g. Debian 11: the security pool now 404s on dpkg, glibc, perl).
# set -eu with one command per line: heredoc scripts don't stop on errors by
# default, and set -e ignores failures on the left of &&, so a failed upgrade
# or install would otherwise ship an unpatched or tini-less image.
ARG OS_UPGRADE=true
RUN <<EOR
    set -eu
    if [ -f /etc/alpine-release ]; then
        if [ "$OS_UPGRADE" = "true" ]; then
            apk upgrade --no-cache
        fi
        apk add --no-cache util-linux zstd squashfs-tools tini
        # isa-l (igzip) needs alpine 3.19+; older bases get pigz
        apk add --no-cache isa-l || apk add --no-cache pigz
    else
        apt-get update
        if [ "$OS_UPGRADE" = "true" ]; then
            DEBIAN_FRONTEND=noninteractive apt-get upgrade -y \
                -o Dpkg::Options::=--force-confdef -o Dpkg::Options::=--force-confold
        fi
        apt-get install -y util-linux zstd squashfs-tools tini pigz
        # isal (igzip) is only packaged on newer debian/ubuntu; pigz covers the rest
        apt-get install -y isal || true
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
