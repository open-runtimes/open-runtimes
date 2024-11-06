# Usage: sh tests.sh node-21.0 (you can also do node-latest, or just node)
set -e

# Configurable varaible for different runtimes
ID=$1
RUNTIME="$(echo $ID | cut -d'-' -f1)"
VERSION="$(echo $ID | cut -d'-' -f2)"
ENTRYPOINT=$(yq ".$RUNTIME.entry" ci/runtimes.toml)
INSTALL_COMMAND=$(yq ".$RUNTIME.commands.install" ci/runtimes.toml)
START_COMMAND=$(yq ".$RUNTIME.commands.start" ci/runtimes.toml)
FORMATTER_CHECK=$(yq ".$RUNTIME.formatter.check" ci/runtimes.toml)
FORMATTER_PREPARE=$(yq ".$RUNTIME.formatter.prepare" ci/runtimes.toml)
TOOLS=$(yq ".$RUNTIME.tools" ci/runtimes.toml)
 
if [ "$VERSION" = "latest"  ] || [ "$VERSION" = "$RUNTIME"  ]; then
    VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
fi

TOOLS=$TOOLS RUNTIME=$RUNTIME ENTRYPOINT=$ENTRYPOINT VERSION=$VERSION INSTALL_COMMAND=$INSTALL_COMMAND START_COMMAND=$START_COMMAND FORMATTER_CHECK=$FORMATTER_CHECK FORMATTER_PREPARE=$FORMATTER_PREPARE bash ci_tests.sh
