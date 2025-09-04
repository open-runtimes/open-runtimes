# Usage: bash tests.sh node-21.0 (you can also do node-latest, or just node)
set -e
shopt -s dotglob

# Configurable varaible for different runtimes
ID=$1
export VERSION="$(echo $ID | sed 's/.*-//')" # Get last section separated by -
export RUNTIME="$(echo $ID | sed 's/\(.*\)-.*/\1/')" # Get all sections separated by - except last line

NEXT_TURBOPACK=false

if [ "$2" = "--turbopack" ]; then
    NEXT_TURBOPACK=true
fi

# If no numbers in version, merge it with runtime name
if ! echo "$VERSION" | grep -q '[0-9]'; then
    # Ignore this logic for 1 word runtimes
    if ! [ "$VERSION" = "$RUNTIME"  ]; then
        RUNTIME="$RUNTIME-$VERSION"
        VERSION="latest"
    fi
fi

# If no version, or version latest, get first runtime's version from list
if [ "$VERSION" = "latest"  ] || [ "$VERSION" = "$RUNTIME"  ]; then
    VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
fi

INSTALL=".$RUNTIME.commands.install"

if [ "$NEXT_TURBOPACK" = true ] && [ "$RUNTIME" = "next-js" ]; then
    INSTALL=".$RUNTIME.commands.install-turbopack"
fi

export ENTRYPOINT=$(yq ".$RUNTIME.entry" ci/runtimes.toml | sed 's/null//')
export INSTALL_COMMAND=$(yq "$INSTALL" ci/runtimes.toml | sed 's/null//')
export START_COMMAND=$(yq ".$RUNTIME.commands.start" ci/runtimes.toml | sed 's/null//')
export FORMATTER_CHECK=$(yq ".$RUNTIME.formatter.check" ci/runtimes.toml | sed 's/null//')
export FORMATTER_PREPARE=$(yq ".$RUNTIME.formatter.prepare" ci/runtimes.toml | sed 's/null//')
export TEST_CLASS=$(yq ".$RUNTIME.test" ci/runtimes.toml | sed 's/null//')
export OUTPUT_DIRECTORY=$(yq ".$RUNTIME.output" ci/runtimes.toml | sed 's/null//')
export TOOLS=$(yq ".$RUNTIME.tools" ci/runtimes.toml | sed 's/null//')
export ENFORCED_RUNTIME=$(yq ".$RUNTIME.runtime.name" ci/runtimes.toml | sed 's/null//')
export ENFORCED_VERSION=$(yq ".$RUNTIME.runtime.version" ci/runtimes.toml | sed 's/null//')

bash ci_tests.sh
