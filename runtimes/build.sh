echo 'Starting local build...'

echo 'Node 17...'
docker build -t open-runtimes/node:17.0 ./node-17.0/

echo 'Deno 1.14...'
docker build -t open-runtimes/deno:1.14 ./deno-1.14/

echo 'Python 3.8...'
docker build -t open-runtimes/python:3.8 ./python-3.8/

echo 'Python 3.9...'
docker build -t open-runtimes/python:3.9 ./python-3.9/

echo 'Python 3.10...'
docker build -t open-runtimes/python:3.10 ./python-3.10/
