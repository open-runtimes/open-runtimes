#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Flutter's build system does not use the environment variables from current process.
# So we export vars to an `.env` file that can be consumed using `--dart-define-from-file`.
printenv | awk -F= '{ print $1 "=\"" substr($0, index($0,$2)) "\"" }' > /usr/local/build/.env