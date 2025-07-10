#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Prepare telemetry
mkdir -p /mnt/telemetry

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Code extraction started. [0m"

# Extract code from mounted volume to function folder
start_ms=$(date +%s%3N)
if [ -f /mnt/code/code.tar ]; then
    tar -xf /mnt/code/code.tar -C /usr/local/server/src/function
elif [ -f /mnt/code/code.tar.gz ] || [ -f /mnt/code/code.gz ]; then
    tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function
else
    echo -e "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Code archive not found. [0m"
    exit 1
fi

elapsed_ms=$(( $(date +%s%3N) - start_ms ))
printf 'extract=%d.%03d\n' $((elapsed_ms/1000)) $((elapsed_ms%1000)) \
    >> /mnt/telemetry/timings.txt

# Apply env vars from build step
set -o allexport
source /usr/local/server/src/function/.open-runtimes
set +o allexport

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Code extraction finished. [0m"

# Enter server folder
cd /usr/local/server

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Environment preparation started. [0m"

. /usr/local/server/helpers/prepare-start.sh

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Environment preparation finished. [0m"

echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][32m Runtime started. [0m"
