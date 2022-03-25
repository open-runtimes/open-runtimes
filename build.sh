echo 'Starting local build...'

echo 'Node 17.0...'
docker build -t openruntimes/node:17.0 ./runtimes/node-17.0/

echo 'Node 16.0...'
docker build -t openruntimes/node:16.0 ./runtimes/node-16.0/

echo 'Node 15.5...'
docker build -t openruntimes/node:15.5 ./runtimes/node-15.5/

echo 'Node 14.5...'
docker build -t openruntimes/node:14.5 ./runtimes/node-14.5/

echo 'Deno 1.14...'
docker build -t openruntimes/deno:1.14 ./runtimes/deno-1.14/

echo 'Deno 1.13...'
docker build -t openruntimes/deno:1.13 ./runtimes/deno-1.13/

echo 'Deno 1.12...'
docker build -t openruntimes/deno:1.12 ./runtimes/deno-1.12/

echo 'Python 3.10...'
docker build -t openruntimes/python:3.10 ./runtimes/python-3.10/

echo 'Python 3.9...'
docker build -t openruntimes/python:3.9 ./runtimes/python-3.9/

echo 'Python 3.8...'
docker build -t openruntimes/python:3.8 ./runtimes/python-3.8/

echo 'Ruby 3.1...'
docker build -t openruntimes/ruby:3.1 ./runtimes/ruby-3.1/

echo 'Ruby 3.0...'
docker build -t openruntimes/ruby:3.0 ./runtimes/ruby-3.0/

echo 'PHP 8.1...'
docker build -t openruntimes/php:8.1 ./runtimes/php-8.1/

echo 'PHP 8.0...'
docker build -t openruntimes/php:8.0 ./runtimes/php-8.0/

echo 'Dart 2.16...'
docker build -t openruntimes/dart:2.16 ./runtimes/dart-2.16/

echo 'Dart 2.15...'
docker build -t openruntimes/dart:2.15 ./runtimes/dart-2.15/

echo 'Dart 2.14...'
docker build -t openruntimes/dart:2.14 ./runtimes/dart-2.14/

echo 'Dart 2.13...'
docker build -t openruntimes/dart:2.13 ./runtimes/dart-2.13/

echo 'Dart 2.12...'
docker build -t openruntimes/dart:2.12 ./runtimes/dart-2.12/

echo 'Swift 5.5...'
docker build -t openruntimes/swift:5.5 ./runtimes/swift-5.5/

echo 'Java 8...'
docker build -t openruntimes/java:8.0 ./runtimes/java-8.0/

echo 'Java 11...'
docker build -t openruntimes/java:11.0 ./runtimes/java-11.0/

echo 'Java 17...'
docker build -t openruntimes/java:17.0 ./runtimes/java-17.0/

echo 'Kotlin 1.6...'
docker build -t openruntimes/kotlin:1.6 ./runtimes/kotlin-1.6/