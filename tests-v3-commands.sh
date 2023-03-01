#!/bin/sh
set -e
cd runtimes/${RUNTIME}
docker build -t open-runtimes/test-runtime .
cd ../../
cd tests/resources/functions/${RUNTIME}
echo "Building..."
docker run --rm --name open-runtimes-test-build -v $(pwd):/usr/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} open-runtimes/test-runtime sh -c "sh helpers/build.sh 'npm install'"
# Main test server
docker run --rm -d --name open-runtimes-test-serve -v $(pwd)/code.tar.gz:/tmp/code.tar.gz:rw -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh 'npm start'"
echo "Waiting for servers..."
sleep 10
cd ../../../../
echo "Running tests..."
OPEN_RUNTIMES_SECRET=test-secret-key OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} vendor/bin/phpunit --configuration phpunit.xml tests/${PHP_CLASS}.php
