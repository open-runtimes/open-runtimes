echo 'Starting local build...'

echo 'Node 17...'
docker build -t open-runtimes/node:17.0 ./runtimes/node-17.0
