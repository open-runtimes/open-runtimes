# Use tests.sh instead, when running locally
set -e

export RUNTIME_FOLDER=$(echo $RUNTIME | sed 's/\(.*\)-.*/\1/') # Get first part separated by -
export VERSION_FOLDER="$RUNTIME-$VERSION"
export VERSION_FOLDER="${VERSION_FOLDER#*-}" # Remove runtime folder name

echo "Preparing Docker image ..."

sh ci-cleanup.sh
sh ci-runtime-prepare.sh
sh ci-runtime-build.sh

LATEST_VERSION=$(yq ".$RUNTIME.versions[0]" ci/runtimes.toml)
if [ "$VERSION" = "$LATEST_VERSION" ]; then
    echo "Running formatter ..."

    cd ./runtimes/$RUNTIME_FOLDER
    docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime sh -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_CHECK"
    cd ../../

    cd "tests/resources/functions/$RUNTIME_FOLDER"
    docker run --rm --name open-runtimes-formatter -v $(pwd):/mnt/code:rw open-runtimes/test-runtime sh -c "cd /mnt/code && $FORMATTER_PREPARE && $FORMATTER_CHECK"
    cd ../../../../
else
    echo "Skipping formatter. Formatter runs only in: $RUNTIME-$LATEST_VERSION"
fi

echo "Running tests ..."
mkdir -p ./tests/.runtime

cp -R ./tests/resources/functions/$RUNTIME_FOLDER/latest/* ./tests/.runtime

if [ -d "./tests/resources/functions/$RUNTIME_FOLDER/$VERSION_FOLDER/" ]; then
    cp -R ./tests/resources/functions/$RUNTIME_FOLDER/$VERSION_FOLDER/* ./tests/.runtime
fi

# Prevent Docker from creating folder
cd ./tests/.runtime
rm -rf code.tar.gz
touch code.tar.gz

# Build
docker run --rm --name open-runtimes-test-build -v /tmp/.build:/usr/local/server/.build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "$BUILD_SCRIPT \"$INSTALL_COMMAND\""

# Edge case for static runtime tests
if [[ "$RUNTIME" == "static" ]]; then
    docker run -d --name open-runtimes-test-serve-main -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_SECRET=test-secret-key -p 3000:3000 open-runtimes/test-runtime sh -c "sh $START_SCRIPT \"$START_COMMAND\""
    cd ../../
    sleep 5
    RUNTIME_NAME="$RUNTIME" RUNTIME_VERSION="$VERSION" OPEN_RUNTIMES_SECRET="test-secret-key" vendor/bin/phpunit --configuration phpunit.xml tests/XStatic.php
    exit 0
fi

# Main tests
docker run -d --name open-runtimes-test-serve-main -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_HEADERS="{\"x-custom\":\"value\",\"X-CUSTOM-UPPERCASE\":\"Value2\",\"x-open-runtimes-custom\":248}" -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh $START_SCRIPT \"$START_COMMAND\""
cd ../../
sleep 30
RUNTIME_NAME="$RUNTIME" RUNTIME_VERSION="$VERSION" OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/$TEST_CLASS

# Customized logging and secret tests
cd ./tests/.runtime
docker run -d --name open-runtimes-test-serve-dev -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_HEADERS= -e OPEN_RUNTIMES_ENV=development -e OPEN_RUNTIMES_SECRET= -e CUSTOM_ENV_VAR=customValue -p 3001:3000 open-runtimes/test-runtime sh -c "sh $START_SCRIPT \"$START_COMMAND\""
cd ../../
sleep 20
vendor/bin/phpunit --configuration phpunit.xml tests/BaseDev.php