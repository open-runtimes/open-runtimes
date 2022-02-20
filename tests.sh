#!/bin/sh
cd runtimes/${RUNTIME}
docker build -t open-runtimes/test-runtime .
cd ../../
cd tests/${RUNTIME}
docker run -d --name open-runtimes-test -v $(pwd):/usr/code:rw -e INTERNAL_RUNTIME_ENTRYPOINT=${ENTRYPOINT} -e ENTRYPOINT_NAME=${ENTRYPOINT} -e INTERNAL_RUNTIME_KEY=test-secret-key -p 3000:3000 open-runtimes/test-runtime sh -c "sh /usr/local/src/build.sh && cp /usr/code/code.tar.gz /tmp/code.tar.gz && sh /usr/local/src/start.sh"
echo "Sleeping to make sure runtime has built and started"
case "${RUNTIME}" in
  *"swift"*)
    sleep 200
    ;;
  *)
    sleep 10
    ;;
esac
docker ps -a
cd ../../
INTERNAL_RUNTIME_KEY=test-secret-key vendor/bin/phpunit --configuration phpunit.xml tests/${PHP_CLASS}.php
