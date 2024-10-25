set -e

AUTH=$(echo -e "$OPEN_RUNTIMES_SECRET" | htpasswd -inBC10 "opr")

static-web-server -p 3000 --log-level info --basic-auth="$AUTH" --page-fallback="/usr/local/server/src/function/index.html" --ignore-hidden-files true --disable-symlinks --compression false -d /usr/local/server/src/function
