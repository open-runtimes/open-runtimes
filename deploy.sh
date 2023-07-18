echo 'Starting deployment...'

if [ -z "$ARCH" ]
then
    echo "ARCH missing, skipping deployment"
else
    docker buildx build --platform ${ARCH} -t ${IMAGE} ./runtimes/${RUNTIME}/ --push
fi
