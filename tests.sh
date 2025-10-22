#!/bin/bash
# Usage: bash tests.sh node-21.0 (you can also do node-latest, or just node)
set -e
shopt -s dotglob

# Configurable variable for different runtimes
ID=$1
VERSION="${ID##*-}" # Get last section separated by -
export VERSION
RUNTIME="${ID%-*}"
export RUNTIME
# Get all sections separated by - except last line

# If no numbers in version, merge it with runtime name
if ! echo "$VERSION" | grep -q '[0-9]'; then
	# Ignore this logic for 1 word runtimes
	if ! [ "$VERSION" = "$RUNTIME" ]; then
		RUNTIME="$RUNTIME-$VERSION"
		VERSION="latest"
	fi
fi

# If no version, or version latest, get first runtime's version from list
if [ "$VERSION" = "latest" ] || [ "$VERSION" = "$RUNTIME" ]; then
	VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
fi

ENTRYPOINT=$(yq ".$RUNTIME.entry" ci/runtimes.toml | sed 's/null//')
export ENTRYPOINT
INSTALL_COMMAND=$(yq ".$RUNTIME.commands.install" ci/runtimes.toml | sed 's/null//')
export INSTALL_COMMAND
START_COMMAND=$(yq ".$RUNTIME.commands.start" ci/runtimes.toml | sed 's/null//')
export START_COMMAND
FORMATTER_CHECK=$(yq ".$RUNTIME.formatter.check" ci/runtimes.toml | sed 's/null//')
export FORMATTER_CHECK
FORMATTER_PREPARE=$(yq ".$RUNTIME.formatter.prepare" ci/runtimes.toml | sed 's/null//')
export FORMATTER_PREPARE
TEST_CLASS=$(yq ".$RUNTIME.test" ci/runtimes.toml | sed 's/null//')
export TEST_CLASS
OUTPUT_DIRECTORY=$(yq ".$RUNTIME.output" ci/runtimes.toml | sed 's/null//')
export OUTPUT_DIRECTORY
TOOLS=$(yq ".$RUNTIME.tools" ci/runtimes.toml | sed 's/null//')
export TOOLS
ENFORCED_RUNTIME=$(yq ".$RUNTIME.runtime.name" ci/runtimes.toml | sed 's/null//')
export ENFORCED_RUNTIME
ENFORCED_VERSION=$(yq ".$RUNTIME.runtime.version" ci/runtimes.toml | sed 's/null//')
export ENFORCED_VERSION

bash ci_tests.sh
