echo 'Starting local build...'

echo 'Node 17...'
docker build -t open-runtimes/node:17.0 ./node-17.0/

echo 'Swift 5.5'
docker build -t open-runtimes/swift-5.5 ./swift-5.5/
