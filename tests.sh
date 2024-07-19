# Usage: sh tests.sh node-21.0
set -e

# Configurable varaible for different runtimes
ID=$1
RUNTIME="$(echo $ID | cut -d'-' -f1)"
VERSION="$(echo $ID | cut -d'-' -f2)"
ENTRYPOINT=$(tq -f ci/runtimes.toml "$RUNTIME.entry")
INSTALL_COMMAND=$(tq -f ci/runtimes.toml "$RUNTIME.commands.install")
START_COMMAND=$(tq -f ci/runtimes.toml "$RUNTIME.commands.start")

RUNTIME=$RUNTIME ENTRYPOINT=$ENTRYPOINT VERSION=$VERSION INSTALL_COMMAND=$INSTALL_COMMAND START_COMMAND=$START_COMMAND sh ci_tests.sh