#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/local/src/src/custom

cd /usr/local/src/src/custom
cargo install --path .

# Rename Main Function Rust
mv /usr/local/src/src/custom/$ENTRYPOINT_NAME /usr/local/src/src/custom/main.rs

# Move to server directory
cd /usr/local/src

# Compile Code
cargo install --path .
cargo build --release

rm -r /usr/builds/*

cp /usr/local/src/target/release/runtime /usr/builds/

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .