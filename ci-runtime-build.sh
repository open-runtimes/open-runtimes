set -e
shopt -s dotglob

cd ./runtimes/.test
docker build -t open-runtimes/test-runtime .
cd ../../