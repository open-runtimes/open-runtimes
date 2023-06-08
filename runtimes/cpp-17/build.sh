#!/usr/bin/env sh

# Fail build if any command fails
set -e

if [ -f "/usr/code/CMakeLists.txt" ]; then
    # Append User Function Dependencies"
    cat "/usr/code/CMakeLists.txt" >> "CMakeLists.txt"
fi

# Prepare separate directory to prevent changing user's files
cp -R --no-clobber /usr/code/* /usr/local/src/src

sed -i "s/{entrypointFile}/$OPEN_RUNTIMES_ENTRYPOINT/g" "src/main.cc"

# Build the executable
rm "CMakeCache.txt" >/dev/null || true
cd /usr/local/src/build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j"$(nproc)"

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .