set -e

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/index.js"
if [ -e "$ENTRYPOINT" ]; then
    mkdir -p .build
    cp -R ./. .build/

    mv .build/ build/

    mv /usr/local/build/package*.json ./
    mv /usr/local/build/node_modules/ ./node_modules/
fi
