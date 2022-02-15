echo 'Starting build...'

echo 'Node 17...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:17.0 ./node-17.0/ --push

echo 'Deno 1.14...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/deno:1.14 ./deno-1.14/ --push

echo 'Ruby 3.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/ruby-3.0 ./ruby-3.0/ --push

echo 'Ruby 3.1...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/ruby-3.1 ./ruby-3.1/ --push
