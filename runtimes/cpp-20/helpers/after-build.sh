#!/bin/sh
# Fail build if any command fails
set -e

# Append User Function Dependencies
if [ -f "/usr/local/build/CMakeLists.txt" ]; then
    cat "/usr/local/build/CMakeLists.txt" >> "/usr/local/server/CMakeLists.txt"
fi

# Copy user code into server code
cp -R --no-clobber /usr/local/build/* /usr/local/server/src

# Update dynamic placeholder to import user code
sed -i "s/{entrypointFile}/$OPEN_RUNTIMES_ENTRYPOINT/g" "/usr/local/server/src/main.cc"

echo "Compiling"

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server

if [ -f "CMakeCache.txt" ]; then
    rm CMakeCache.txt
fi

mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j"$(nproc)"
mv /usr/local/server/build/cpp_runtime /usr/local/build/compiled/cpp_runtime

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."