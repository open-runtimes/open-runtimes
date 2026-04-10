#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

CPUS=$(nproc 2>/dev/null || echo 2)
WORKERS=$((CPUS))
THREADS=16

bundle exec puma -b tcp://0.0.0.0:3000 -e production -w $WORKERS -t $THREADS:$THREADS
