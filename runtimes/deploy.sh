echo 'Starting build...'

echo 'Node 17.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:17.0 ./node-17.0/ --push

echo 'Deno 1.14...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/deno:1.14 ./deno-1.14/ --push

echo 'Python 3.10...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.10 ./python-3.10/ --push

echo 'Ruby 3.1...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/ruby-3.1 ./ruby-3.1/ --push

echo 'PHP 8.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/php:8.0 ./php-8.0/ --push

echo 'Dart 2.15...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/dart:2.15 ./dart-2.15/ --push
