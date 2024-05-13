# Configurable varaible for different runtimes
RUNTIME="node-21.0" # Folder name
ENTRYPOINT="tests.js" # Test file file
INSTALL_COMMAND="npm install" # Build script
START_COMMAND="pm2 start src/server.js --no-daemon" # Run script

RUNTIME="deno-1.21" # Folder name
ENTRYPOINT="tests.ts" # Test file file
INSTALL_COMMAND="deno cache tests.ts" # Build script
START_COMMAND="denon start" # Run script

RUNTIME="bun-1.0" # Folder name
ENTRYPOINT="tests.ts" # Test file file
INSTALL_COMMAND="bun install" # Build script
START_COMMAND="bun src/server.ts" # Run script

RUNTIME="dart-3.3" # Folder name
ENTRYPOINT="lib/tests.dart" # Test file file
INSTALL_COMMAND="dart pub get" # Build script
START_COMMAND="src/function/server" # Run script

RUNTIME="python-3.8" # Folder name
ENTRYPOINT="tests.py" # Test file file
INSTALL_COMMAND="pip install --no-cache-dir -r requirements.txt" # Build script
START_COMMAND="python3 src/server.py" # Run script

RUNTIME="ruby-3.3" # Folder name
ENTRYPOINT="tests.rb" # Test file file
INSTALL_COMMAND="" # Build script
START_COMMAND="bundle exec puma -b tcp://0.0.0.0:3000 -e production" # Run script

RUNTIME="java-18.0" # Folder name
ENTRYPOINT="Tests.java" # Test file file
INSTALL_COMMAND="" # Build script
START_COMMAND="java -jar src/function/java-runtime-1.0.0.jar" # Run script

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

# Build and start
docker run --rm --name open-runtimes-test-build -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" open-runtimes/test-runtime sh -c "sh helpers/build.sh \"$INSTALL_COMMAND\""
docker run -d --rm --name open-runtimes-test-serve -v $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:rw -e OPEN_RUNTIMES_ENTRYPOINT="$ENTRYPOINT" -e OPEN_RUNTIMES_SECRET=test-secret-key -e CUSTOM_ENV_VAR=customValue -p 3000:3000 open-runtimes/test-runtime sh -c "sh helpers/start.sh \"$START_COMMAND\""
sleep 10
cd ../../../../

# Run tests
OPEN_RUNTIMES_SECRET="test-secret-key" OPEN_RUNTIMES_ENTRYPOINT=$ENTRYPOINT vendor/bin/phpunit --configuration phpunit.xml tests/Base.php