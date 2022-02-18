echo 'Starting local build...'

echo 'Node 17.0...'
docker build -t open-runtimes/node:17.0 ./node-17.0/

echo 'Node 16.0...'
docker build -t open-runtimes/node:16.0 ./node-16.0/

echo 'Node 15.5...'
docker build -t open-runtimes/node:15.5 ./node-15.5/

echo 'Node 14.5...'
docker build -t open-runtimes/node:14.5 ./node-14.5/

echo 'Deno 1.14...'
docker build -t open-runtimes/deno:1.14 ./deno-1.14/

echo 'Deno 1.13...'
docker build -t open-runtimes/deno:1.13 ./deno-1.13/

echo 'Deno 1.12...'
docker build -t open-runtimes/deno:1.12 ./deno-1.12/

echo 'Python 3.10...'
docker build -t open-runtimes/python:3.10 ./python-3.10/

echo 'Python 3.9...'
docker build -t open-runtimes/python:3.9 ./python-3.9/

echo 'Python 3.8...'
docker build -t open-runtimes/python:3.8 ./python-3.8/

echo 'Ruby 3.1...'
docker build -t open-runtimes/ruby:3.1 ./ruby-3.1/

echo 'Ruby 3.0...'
docker build -t open-runtimes/ruby:3.0 ./ruby-3.0/

echo 'PHP 8.1...'
docker build -t open-runtimes/php:8.1 ./php-8.1/

echo 'PHP 8.0...'
docker build -t open-runtimes/php:8.0 ./php-8.0/

echo 'Dart 2.15...'
docker build -t open-runtimes/dart:2.15 ./dart-2.15/

echo 'Dart 2.14...'
docker build -t open-runtimes/dart:2.14 ./dart-2.14/

echo 'Dart 2.13...'
docker build -t open-runtimes/dart:2.13 ./dart-2.13/

echo 'Dart 2.12...'
docker build -t open-runtimes/dart:2.12 ./dart-2.12/