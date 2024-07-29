set -e

if [ "$(docker ps -aq)" ]; then
    docker rm --force $(docker ps -aq)
fi

rm -rf /tmp/logs
mkdir -p /tmp/logs

rm -rf ./runtimes/.test

rm -rf ./tests/.runtime