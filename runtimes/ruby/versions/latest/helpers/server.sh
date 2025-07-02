#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

bundle exec puma -b tcp://0.0.0.0:3000 -e production
