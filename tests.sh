# Usage: sh tests.sh node-21.0

# Configurable varaible for different runtimes
ID=$1
ENTRYPOINT=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .ENTRYPOINT" .github/workflows/test.yaml | head -n 1)
INSTALL_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .INSTALL_COMMAND" .github/workflows/test.yaml | head -n 1)
START_COMMAND=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .START_COMMAND" .github/workflows/test.yaml | head -n 1)
RUNTIME=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .RUNTIME" .github/workflows/test.yaml | head -n 1)
VERSION=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .VERSION" .github/workflows/test.yaml | head -n 1)
FORMATTER=$(yq e ".jobs.open-runtimes.strategy.matrix.include[] | select(.ID == \"$ID\") | .FORMATTER" .github/workflows/test.yaml | head -n 1)

RUNTIME=$RUNTIME ENTRYPOINT=$ENTRYPOINT VERSION=$VERSION INSTALL_COMMAND=$INSTALL_COMMAND START_COMMAND=$START_COMMAND FORMATTER=$FORMATTER sh ci_tests.sh
