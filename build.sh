echo 'Starting local build...'

echo 'C++ 2017...'
docker build -t openruntimes/cpp:v2-17 ./runtimes/cpp-17/

echo 'Dart 2.15...'
docker build -t openruntimes/dart:v2-2.15 ./runtimes/dart-2.15/

echo 'Dart 2.16...'
docker build -t openruntimes/dart:v2-2.16 ./runtimes/dart-2.16/

echo 'Dart 2.17...'
docker build -t openruntimes/dart:v2-2.17 ./runtimes/dart-2.17/

echo 'Deno 1.21...'
docker build -t openruntimes/deno:v2-1.21 ./runtimes/deno-1.21/

echo 'Deno 1.24...'
docker build -t openruntimes/deno:v2-1.24 ./runtimes/deno-1.24/

echo '.NET 3.1...'
docker build -t openruntimes/dotnet:v2-3.1 ./runtimes/dotnet-3.1/

echo '.NET 6.0...'
docker build -t openruntimes/dotnet:v2-6.0 ./runtimes/dotnet-6.0/

echo 'Perl 5.36...'
docker build -t openruntimes/perl-5.36 ./runtimes/perl-5.36/

echo 'Java 8...'
docker build -t openruntimes/java:v2-8.0 ./runtimes/java-8.0/

echo 'Java 11...'
docker build -t openruntimes/java:v2-11.0 ./runtimes/java-11.0/

echo 'Java 17...'
docker build -t openruntimes/java:v2-17.0 ./runtimes/java-17.0/

echo 'Kotlin 1.6...'
docker build -t openruntimes/kotlin:v2-1.6 ./runtimes/kotlin-1.6/

echo 'Node 14.5...'
docker build -t openruntimes/node:v2-14.5 ./runtimes/node-14.5/

echo 'Node 16.0...'
docker build -t openruntimes/node:v2-16.0 ./runtimes/node-16.0/

echo 'Node 18.0...'
docker build -t openruntimes/node:v2-18.0 ./runtimes/node-18.0/

echo 'PHP 8.0...'
docker build -t openruntimes/php:v2-8.0 ./runtimes/php-8.0/

echo 'PHP 8.1...'
docker build -t openruntimes/php:v2-8.1 ./runtimes/php-8.1/

echo 'Python 3.8...'
docker build -t openruntimes/python:v2-3.8 ./runtimes/python-3.8/

echo 'Python 3.9...'
docker build -t openruntimes/python:v2-3.9 ./runtimes/python-3.9/

echo 'Python 3.10...'
docker build -t openruntimes/python:v2-3.10 ./runtimes/python-3.10/

echo 'Ruby 3.0...'
docker build -t openruntimes/ruby:v2-3.0 ./runtimes/ruby-3.0/

echo 'Ruby 3.1...'
docker build -t openruntimes/ruby:v2-3.1 ./runtimes/ruby-3.1/

echo 'Swift 5.5...'
docker build -t openruntimes/swift:v2-5.5 ./runtimes/swift-5.5/
