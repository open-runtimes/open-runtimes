#!/bin/bash
set -e
shopt -s dotglob

RUNTIME_FOLDER="${RUNTIME%-*}" # Get first part separated by -
export RUNTIME_FOLDER
export VERSION_FOLDER="$RUNTIME-$VERSION"
export VERSION_FOLDER="${VERSION_FOLDER#*-}" # Remove runtime folder name

RUNTIME_OVERRIDE=$(yq ".$RUNTIME.runtime.name" ci/runtimes.toml | sed 's/null//')
if [ -n "$RUNTIME_OVERRIDE" ]; then
	export RUNTIME_FOLDER="$RUNTIME_OVERRIDE"
fi

VERSION_OVERRIDE=$(yq ".$RUNTIME.runtime.version" ci/runtimes.toml | sed 's/null//')
if [ -n "$VERSION_OVERRIDE" ]; then
	export VERSION_FOLDER="$VERSION_OVERRIDE"
fi
