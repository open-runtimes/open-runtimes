# Usage: sh tests.sh node-21.0

# Exit script on any error
set -e

# Pre-checks
if ! command -v yq &> /dev/null; then
    echo "yq could not be found, please install yq. See - https://github.com/mikefarah/yq"
    exit 1
fi

if ! command -v docker &> /dev/null; then
    echo "Docker is not installed or running!"
    exit 1
fi

if ! command -v timeout &> /dev/null; then
    echo "timeout is not available."
    exit 1
fi

RUNTIME=$1
if [ -z "$RUNTIME" ]; then
    echo "Usage: $0 <runtime>"
    exit 1
fi

# Extract configurations using `yq` from the GitHub Actions workflow file
ENTRYPOINT=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .ENTRYPOINT" .github/workflows/test.yaml | head -n 1)
INSTALL_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .OPEN_RUNTIMES_BUILD_COMMAND" .github/workflows/test.yaml | head -n 1)
START_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .OPEN_RUNTIMES_START_COMMAND" .github/workflows/test.yaml | head -n 1)

# Cleanup
containers=$(docker ps -aq)
if [ -n "$containers" ]; then
  docker rm --force $containers
fi

# Prepare image
cd ./runtimes/$RUNTIME
docker build -t open-runtimes/test-runtime .
cd ../../

# Prevent Docker from creating folder
cd ./tests/resources/functions/$RUNTIME
rm -rf code.tar.gz
touch code.tar.gz

# Build and start
docker run --rm --name open-runtimes-test-build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$INSTALL_COMMAND\""
docker run -d --rm --name open-runtimes-test-serve -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
cd ../../../../

## Wait for server to be ready
while ! timeout 0.5 bash -c "</dev/tcp/127.0.0.1/3000"; do sleep 0.5; done
sleep 1

# Run tests
RUNTIME=$RUNTIME OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/Base.php