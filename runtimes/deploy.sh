echo 'Starting build...'

echo 'Node 17...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:17.0 ./node-17.0/ --push

echo 'Python 3.8...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.8 ./python-3.8/ --push

echo 'Python 3.9...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.9 ./python-3.9/ --push

echo 'Python 3.10...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.10 ./python-3.10/ --push
