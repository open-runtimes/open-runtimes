echo 'Starting build...'

echo 'Node 17.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:17.0 ./node-17.0/ --push

echo 'Node 16.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:16.0 ./node-16.0/ --push

echo 'Node 15.5...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:15.5 ./node-15.5/ --push

echo 'Node 14.5...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/node:14.5 ./node-14.5/ --push

echo 'Deno 1.14...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/deno:1.14 ./deno-1.14/ --push

echo 'Deno 1.13...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/deno:1.13 ./deno-1.13/ --push

echo 'Deno 1.12...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/deno:1.12 ./deno-1.12/ --push

echo 'Python 3.10...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.10 ./python-3.10/ --push

echo 'Python 3.9...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.9 ./python-3.9/ --push

echo 'Python 3.8...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/python:3.8 ./python-3.8/ --push

echo 'Ruby 3.1...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/ruby-3.1 ./ruby-3.1/ --push

echo 'Ruby 3.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/ruby-3.0 ./ruby-3.0/ --push

echo 'PHP 8.1...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/php:8.1 ./php-8.1/ --push

echo 'PHP 8.0...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/php:8.0 ./php-8.0/ --push

echo 'Dart 2.15...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/dart:2.15 ./dart-2.15/ --push

echo 'Dart 2.14...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/dart:2.14 ./dart-2.14/ --push

echo 'Dart 2.13...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/dart:2.13 ./dart-2.13/ --push

echo 'Dart 2.12...'
docker buildx build --platform linux/amd64,linux/arm/v6,linux/arm/v7,linux/arm64,linux/ppc64le -t open-runtimes/dart:2.12 ./dart-2.12/ --push
