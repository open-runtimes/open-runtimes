
echo 'Starting build test...'
docker build -t ${IMAGE} ./runtimes/${RUNTIME}/ && docker push ${IMAGE}

echo 'Starting build...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t ${IMAGE} ./runtimes/${RUNTIME}/ --push