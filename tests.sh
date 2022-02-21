#!/bin/sh
cd runtimes/${RUNTIME}
docker build -t open-runtimes/test-runtime .
cd ../../
cd tests/${RUNTIME}
docker run -d --name open-runtimes-test -v $(pwd):/usr/code:rw -e INTERNAL_RUNTIME_ENTRYPOINT=${ENTRYPOINT} -e ENTRYPOINT_NAME=${ENTRYPOINT} -e INTERNAL_RUNTIME_KEY=test-secret-key -p 3000:3000 open-runtimes/test-runtime sh -c "sh /usr/local/src/build.sh && cp /usr/code/code.tar.gz /tmp/code.tar.gz && sh /usr/local/src/start.sh"
echo "Waiting for server..."
max_wait=500
wait_interval=10
wait_count=0
while [ -z "$(docker container top open-runtimes-test | grep "${SERVER_PROCESS}")" ]; do
    [ $wait_count -gt $max_wait ] && echo "Server failed to start" && exit 1
    wait_count=$((wait_count + wait_interval))
    sleep $wait_interval
done
sleep $wait_interval
cd ../../
INTERNAL_RUNTIME_KEY=test-secret-key vendor/bin/phpunit --configuration phpunit.xml tests/${PHP_CLASS}.php