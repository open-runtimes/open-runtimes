# Usage: sh formatter.sh node
set -e
shopt -s dotglob

# Configurable varaible for different runtimes
export RUNTIME=$1
export VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
export ID="$RUNTIME-$VERSION"
export FORMATTER_WRITE=$(yq ".$RUNTIME.formatter.write" ci/runtimes.toml)
export FORMATTER_PREPARE=$(yq ".$RUNTIME.formatter.prepare" ci/runtimes.toml)
export RUNTIME_FOLDER=$RUNTIME
export VERSION_FOLDER=$VERSION

echo "Preparing Docker image ..."

bash ci-cleanup.sh
bash ci-runtime-prepare.sh
bash ci-runtime-build.sh

cd "runtimes/$RUNTIME"
docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime bash -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_WRITE"
cd ../../

if [ -d "tests/resources/functions/$RUNTIME" ]; then
    cd "tests/resources/functions/$RUNTIME"
    docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime bash -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_WRITE"
    cd ../../../../
fi