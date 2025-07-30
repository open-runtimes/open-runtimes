#!/bin/bash
set -e

# Usage: ./scripts/rollback.sh <runtime> <version> <commit-sha>
RUNTIME=$1
VERSION=$2
COMMIT_SHA=$3

if [ -z "$RUNTIME" ] || [ -z "$VERSION" ] || [ -z "$COMMIT_SHA" ]; then
    echo "Usage: $0 <runtime> <version> <commit-sha>"
    echo "Example: $0 node 22 abc123def"
    exit 1
fi

IMAGE="openruntimes/$RUNTIME"

echo "Rolling back $IMAGE:v5-$VERSION to commit $COMMIT_SHA..."

# Pull the specific commit hash
docker pull $IMAGE:$COMMIT_SHA

# Create new version tag pointing to the commit
docker tag $IMAGE:$COMMIT_SHA $IMAGE:v5-$VERSION

# Push the rollback
docker push $IMAGE:v5-$VERSION

echo "Rollback completed: $IMAGE:v5-$VERSION now points to commit $COMMIT_SHA" 