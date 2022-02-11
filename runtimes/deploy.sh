echo 'Starting build...'

echo 'Node 17...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:17.0 ./node-17.0/ --push
