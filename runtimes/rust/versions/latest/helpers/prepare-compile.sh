#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Compiling Rust function..."

# Copy user code to function directory
cp -r /usr/local/build/* /usr/local/server/src/function/

# Compile the code
cd /usr/local/server
cargo build --release

# Move binary to build directory
mv /usr/local/server/target/release/rust_runtime /usr/local/build/rust_runtime
