#!/bin/sh

# Build runtime Docker image
cd runtimes/${RUNTIME}
docker build --load -t open-runtimes/test-runtime .

# Enter function folder
cd ../../
cd tests/resources/functions/${RUNTIME}

# Prevent Docker mount from creating directory
rm -rf code.tar.gz
touch code.tar.gz

# Build and start runtime
docker run --rm --name open-runtimes-test-build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$OPEN_RUNTIMES_BUILD_COMMAND\""
docker run -d --rm --name open-runtimes-test-serve -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$OPEN_RUNTIMES_START_COMMAND\""

# Wait for runtime to start
echo "Waiting for servers..."
sleep 10

# Run tests
cd ../../../../
echo "Running tests..."
OPEN_RUNTIMES_SECRET=test-secret-key OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} vendor/bin/phpunit --configuration phpunit.xml tests/${TEST_CLASS}.php
