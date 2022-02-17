echo 'Starting local build...'

echo 'Node 17...'
docker build -t open-runtimes/node:17.0 ./node-17.0/

echo 'Deno 1.14...'
docker build -t open-runtimes/deno:1.14 ./deno-1.14/

echo 'Python 3.10...'
docker build -t open-runtimes/python:3.10 ./python-3.10/

echo 'Ruby 3.1...'
docker build -t open-runtimes/ruby:3.1 ./ruby-3.1/

echo 'PHP 8.0...'
docker build -t open-runtimes/php:8.0 ./php-8.0/

echo 'Swift 5.5'
docker build -t open-runtimes/swift-5.5 ./swift-5.5/

