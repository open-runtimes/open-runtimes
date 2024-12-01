set -e

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

mv /usr/local/build/package*.json server/
mv /usr/local/build/node_modules/ server/node_modules/
