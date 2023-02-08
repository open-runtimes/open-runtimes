#!/bin/sh
set -e
cd runtimes/${RUNTIME}
docker build -t open-runtimes/test-runtime .
cd ../../
cd tests/resources/functions/${RUNTIME}
echo "Building..."
touch code.tar.gz
tar --exclude code.tar.gz -czf code.tar.gz .
# Main test server
docker run --rm --name open-runtimes-test-build -v $(pwd)/code.tar.gz:/usr/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} open-runtimes/test-runtime sh -c "tar -xzf /usr/code/code.tar.gz -C /usr/code && cp -R /usr/code/* /usr/builds && cd /usr/builds && if [ -f package.json ]; then npm install; fi && mkdir -p node_modules && cp -R /usr/local/src/node_modules/* /usr/builds/node_modules && tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz ."
docker run --rm -d --name open-runtimes-test-serve -v $(pwd):/usr/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "cp /usr/code/code.tar.gz /tmp/code.tar.gz && cp /tmp/code.tar.gz /usr/workspace/code.tar.gz && cd /usr/workspace && tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start && rm /usr/workspace/code.tar.gz && cp -R /usr/code-start/node_modules/* /usr/local/src/node_modules && cd /usr/local/src && npm start"
echo "Waiting for servers..."
sleep 10
cd ../../../../
echo "Running tests..."
OPEN_RUNTIMES_SECRET=test-secret-key OPEN_RUNTIMES_ENTRYPOINT=${ENTRYPOINT} vendor/bin/phpunit --configuration phpunit.xml tests/${PHP_CLASS}.php
