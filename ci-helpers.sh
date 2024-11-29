set -e

export RUNTIME_FOLDER=$(echo $RUNTIME | sed 's/\(.*\)-.*/\1/') # Get first part separated by -
export VERSION_FOLDER="$RUNTIME-$VERSION"
export VERSION_FOLDER="${VERSION_FOLDER#*-}" # Remove runtime folder name

if ! [ -z "$ENFORCED_RUNTIME" ]; then
    export RUNTIME_FOLDER="$ENFORCED_RUNTIME"
fi

if ! [ -z "$ENFORCED_VERSION" ]; then
    export VERSION_FOLDER="$ENFORCED_VERSION"
fi
