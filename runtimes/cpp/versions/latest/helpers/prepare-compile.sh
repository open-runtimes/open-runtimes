#!/bin/sh

# Append User Function Dependencies
if [ -f "/usr/local/build/CMakeLists.txt" ]; then
    cat "/usr/local/build/CMakeLists.txt" >> "/usr/local/server/CMakeLists.txt"
fi

# Copy user code into server code
cp -R --no-clobber /usr/local/build/* /usr/local/server/src

# Update dynamic placeholder to import user code
ESCAPED_OPEN_RUNTIMES_ENTRYPOINT="$(echo "$OPEN_RUNTIMES_ENTRYPOINT" | sed -e 's/[\/&]/\\&/g')"
sed -i "s/{entrypointFile}/$ESCAPED_OPEN_RUNTIMES_ENTRYPOINT/g" "/usr/local/server/src/main.cc"

echo "Compiling ..."

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
