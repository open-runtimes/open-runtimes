#!/bin/sh
cd runtimes/${RUNTIME}
docker build -t open-runtimes/test-runtime .
cd ../../
cd tests/${RUNTIME}
docker run -d --name open-runtimes-test -v $(pwd):/usr/code:ro -e ENTRYPOINT_NAME=${ENTRYPOINT} -e INTERNAL_RUNTIME_KEY=test-secret-key -p 3000:3000 open-runtimes/test-runtime sh -c "sh /usr/local/src/build.sh && sh /usr/local/src/start.sh"
echo "Sleeping for 200 seconds, to make sure runtime has built and started"
sleep 200
cd ../..
INTERNAL_RUNTIME_KEY=test-secret-key vendor/bin/phpunit tests/${PHP_CLASS}.php