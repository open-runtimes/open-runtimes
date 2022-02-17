#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/builds

# Copy User Code
cp -a /usr/builds/. /usr/local/src/src/custom

# Rename Main Function Rust
mv /usr/local/src/src/custom/$ENTRYPOINT_NAME /usr/local/src/src/custom/main.rs

# Compile Code
cd /usr/local/src
cargo build --release

# Copy compiled files into build
cp /usr/local/src/target/release/runtime /usr/builds/
cp /usr/local/src/Rocket.toml /usr/builds/

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .