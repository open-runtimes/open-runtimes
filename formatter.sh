# Usage: sh formatter.sh node
set -e

# Configurable varaible for different runtimes
export RUNTIME=$1
export VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
export ID="$RUNTIME-$VERSION"
export FORMATTER_WRITE=$(yq ".$RUNTIME.formatter.write" ci/runtimes.toml)
export FORMATTER_PREPARE=$(yq ".$RUNTIME.formatter.prepare" ci/runtimes.toml)

echo "Preparing Docker image ..."

sh ci-cleanup.sh
sh ci-runtime-prepare.sh
sh ci-runtime-build.sh

cd "runtimes/$RUNTIME"
docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime sh -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_WRITE"
cd ../../

cd "tests/resources/functions/$RUNTIME"
docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime sh -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_WRITE"

cd ../../../../