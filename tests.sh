# Usage: sh tests.sh node-21.0

# Configurable varaible for different runtimes
RUNTIME=$1
ENTRYPOINT=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .ENTRYPOINT" .github/workflows/test.yaml | head -n 1)
INSTALL_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .OPEN_RUNTIMES_BUILD_COMMAND" .github/workflows/test.yaml | head -n 1)
START_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.RUNTIME == \"$RUNTIME\") | .OPEN_RUNTIMES_START_COMMAND" .github/workflows/test.yaml | head -n 1)

# Cleanup
docker rm --force $(docker ps -aq)
rm -rf /tmp/logs
mkdir -p /tmp/logs

# Prepare image
cd ./runtimes/$RUNTIME
docker build -t open-runtimes/test-runtime .
cd ../../

# Prevent Docker from creating folder
cd ./tests/resources/functions/$RUNTIME
rm -rf code.tar.gz
touch code.tar.gz

# Build
docker run --rm --name open-runtimes-test-build -v /tmp/.build:/usr/local/server/.build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$INSTALL_COMMAND\""

# Main tests
docker run -d --name open-runtimes-test-serve -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_HEADERS="{\"x-custom\":\"value\",\"x-open-runtimes-custom\":248}" -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
cd ../../../../
sleep 10
OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/Base.php

# Dev tests (Uncomment only if fails on CI/CD. Changes rarely make them fail)

docker rm --force $(docker ps -aq)
cd ./tests/resources/functions/$RUNTIME
docker run -d --name open-runtimes-test-serve -v /tmp/logs:/mnt/logs -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_ENV=development -e OPEN_RUNTIMES_SECRET= -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
cd ../../../../
sleep 10
vendor/bin/phpunit --configuration phpunit.xml tests/BaseDev.php
