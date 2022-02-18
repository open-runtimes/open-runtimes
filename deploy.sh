echo 'Starting build...'
docker buildx build --platform ${ARCH} -t ${IMAGE} ./runtimes/${RUNTIME}/ --push