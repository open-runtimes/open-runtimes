# Usage: sh tests.sh node-21.0
set -e

# Configurable varaible for different runtimes
ID=$1
ENTRYPOINT=$(tq -f ci/runtimes.toml 'go.entry')
INSTALL_COMMAND=$(tq -f ci/runtimes.toml 'go.commands.install')
START_COMMAND=$(tq -f ci/runtimes.toml 'go.commands.start')
RUNTIME="$(echo $ID | cut -d'-' -f1)"
VERSION="$(echo $ID | cut -d'-' -f2)"

RUNTIME=$RUNTIME ENTRYPOINT=$ENTRYPOINT VERSION=$VERSION INSTALL_COMMAND=$INSTALL_COMMAND START_COMMAND=$START_COMMAND sh ci_tests.sh