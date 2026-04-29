#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

src/function/rust_runtime
