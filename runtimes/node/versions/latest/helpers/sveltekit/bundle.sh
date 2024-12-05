set -e

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

# Install production dependencies only for SSR
mv /usr/local/build/package*.json ./
npm ci --production
