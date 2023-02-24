#!/bin/sh
set -e
cd runtimes/${RUNTIME}
docker build --output type=docker -t open-runtimes/test-runtime .
cd ../../
cd tests/${RUNTIME}
echo "Building..."
touch code.tar.gz
tar --exclude code.tar.gz -czf code.tar.gz .
docker run --rm --name open-runtimes-test-build -v $(pwd)/code.tar.gz:/usr/code/code.tar.gz:rw -e INTERNAL_RUNTIME_ENTRYPOINT=${ENTRYPOINT} -e INTERNAL_RUNTIME_KEY=test-secret-key open-runtimes/test-runtime sh -c "tar -xzf /usr/code/code.tar.gz -C /usr/code && sh /usr/local/src/build.sh"
docker run --rm -d --name open-runtimes-test-serve -v $(pwd):/usr/code:rw -e INTERNAL_RUNTIME_ENTRYPOINT=${ENTRYPOINT} -e INTERNAL_RUNTIME_KEY=test-secret-key -p 3000:3000 open-runtimes/test-runtime sh -c "cp /usr/code/code.tar.gz /tmp/code.tar.gz && sh /usr/local/src/start.sh"
echo "Waiting for server..."
max_wait=500
wait_interval=10
wait_count=0
# while [ -z "$(docker container top open-runtimes-test-serve | grep "${SERVER_PROCESS}")" ]; do
#     [ $wait_count -gt $max_wait ] && echo "Server failed to start" && exit 1
#     wait_count=$((wait_count + wait_interval))
#     sleep $wait_interval
# done
sleep $wait_interval
cd ../../
echo "Running tests..."
INTERNAL_RUNTIME_KEY=test-secret-key vendor/bin/phpunit --configuration phpunit.xml tests/${PHP_CLASS}.php
