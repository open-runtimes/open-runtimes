#!/bin/bash
set -e
shopt -s dotglob

mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

# Shared family files (e.g., runtimes/javascript/*) — overlaid before the
# runtime-specific files so anything in runtimes/<runtime>/versions/... still
# wins on conflict. Opt-in per runtime via the `shared` property in
# ci/runtimes.toml.
SHARED=$(yq ".$RUNTIME_FOLDER.shared" ci/runtimes.toml | sed 's/null//')
if [ -n "$SHARED" ] && [ -d "./runtimes/$SHARED" ]; then
	cp -R "./runtimes/$SHARED"/* ./runtimes/.test/
fi

# Runtime base dockerfile
cp -R "./runtimes/$RUNTIME_FOLDER/$RUNTIME_FOLDER.dockerfile" ./runtimes/.test

# Runtime-specific files (most)
cp -R "./runtimes/$RUNTIME_FOLDER/versions/latest"/* ./runtimes/.test

# Version-specific files
cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION_FOLDER"/* ./runtimes/.test

# Global Docker configuration
cp ./base-before.dockerfile ./runtimes/.test/base-before.dockerfile
cp ./base-after.dockerfile ./runtimes/.test/base-after.dockerfile
