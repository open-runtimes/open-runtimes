# Use tests.sh instead, when running locally

sh ci-cleanup.sh
sh ci-runtime-prepare.sh
sh ci-runtime-build.sh

# Find test folder (versioned takes presence)
TEST_FOLDER="./tests/resources/functions/$RUNTIME-$VERSION/"
if [ -d "$TEST_FOLDER" ]; then
    TEST_FOLDER="./tests/resources/functions/$RUNTIME-$VERSION"
else
    TEST_FOLDER="./tests/resources/functions/$RUNTIME"
fi

mkdir -p ./tests/.runtime

cp -R ./tests/resources/functions/$RUNTIME/latest/* ./tests/.runtime
cp -R ./tests/resources/functions/$RUNTIME/$VERSION/* ./tests/.runtime

# Prevent Docker from creating folder
cd ./tests/.runtime
rm -rf code.tar.gz
touch code.tar.gz

# Build
docker run --rm --name open-runtimes-test-build -v /tmp/.build:/usr/local/server/.build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$INSTALL_COMMAND\""

# Main tests
docker run -d --name open-runtimes-test-serve -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_HEADERS="{\"x-custom\":\"value\",\"X-CUSTOM-UPPERCASE\":\"value2\",\"x-open-runtimes-custom\":248}" -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
cd ../../
sleep 10
OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/Base.php

# Dev tests

docker rm --force $(docker ps -aq)
cd ./tests/.runtime
docker run -d --name open-runtimes-test-serve -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_HEADERS= -e OPEN_RUNTIMES_ENV=development -e OPEN_RUNTIMES_SECRET= -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
cd ../../
sleep 10
vendor/bin/phpunit --configuration phpunit.xml tests/BaseDev.php
