set -e
shopt -s dotglob

# Remove only open-runtimes test containers
containers=(
    "open-runtimes-formatter"
    "open-runtimes-test-build"
    "open-runtimes-test-tools"
    "open-runtimes-test-serve"
    "open-runtimes-test-serve-secondary"
    "open-runtimes-test-serve-teritary"
)

for container in "${containers[@]}"; do
    if [ "$(docker ps -aq -f name=$container)" ]; then
        docker rm --force $(docker ps -aq -f name=$container)
    fi
done

rm -rf /tmp/logs
mkdir -p /tmp/logs

rm -rf ./runtimes/.test

rm -rf ./tests/.runtime