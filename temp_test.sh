# TODO: Delete me and add me to proper CI/CD test

# Configurable varaible for different runtimes
RUNTIME="go-1.22" # Folder name
ENTRYPOINT="tests.go" # Test file file
INSTALL_COMMAND="" # Build script
START_COMMAND="/usr/local/server/src/function/server" # Run script

# Cleanup
docker rm --force $(docker ps -aq)

# Prepare image
cd ./runtimes/$RUNTIME
docker build -t open-runtimes/test-runtime .
cd ../../

# Prevent Docker from creating folder
cd ./tests/resources/functions/$RUNTIME
rm -rf code.tar.gz
touch code.tar.gz

# Build and start
docker run --rm --name open-runtimes-test-build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$INSTALL_COMMAND\""
docker run -d --rm --name open-runtimes-test-serve -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
sleep 10
cd ../../../../

# Run tests
OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/Base.php