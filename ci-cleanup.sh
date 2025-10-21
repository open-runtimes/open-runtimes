set -e
shopt -s dotglob

if [ "$(docker ps -aq -f name=open-runtimes-)" ]; then
	docker rm --force $(docker ps -aq -f name=open-runtimes-)
fi

rm -rf /tmp/logs
mkdir -p /tmp/logs

rm -rf ./runtimes/.test

rm -rf ./tests/.runtime
