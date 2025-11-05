#!/bin/bash
set -e
shopt -s dotglob

source ci-helpers.sh
bash ci-cleanup.sh
bash ci-runtime-prepare.sh
