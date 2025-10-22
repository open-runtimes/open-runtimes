#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

php -d memory_limit=8G src/server.php
