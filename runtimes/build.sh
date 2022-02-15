echo 'Starting local build...'

echo 'Node 17...'
docker build -t open-runtimes/node:17.0 ./node-17.0/

echo 'Deno 1.14...'
docker build -t open-runtimes/deno:1.14 ./deno-1.14/