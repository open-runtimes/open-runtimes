set -e

export RUNTIME_FOLDER=$(echo $RUNTIME | sed 's/\(.*\)-.*/\1/') # Get first part separated by -
export VERSION_FOLDER="$RUNTIME-$VERSION"
export VERSION_FOLDER="${VERSION_FOLDER#*-}" # Remove runtime folder name

sh ci-cleanup.sh
sh ci-runtime-prepare.sh