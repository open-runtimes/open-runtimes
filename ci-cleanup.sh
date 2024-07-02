docker rm --force $(docker ps -aq)

rm -rf /tmp/logs
mkdir -p /tmp/logs

rm -rf ./runtimes/.test

rm -rf ./tests/.runtime