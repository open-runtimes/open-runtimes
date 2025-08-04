#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Preparing for start ..."

# Extract gzipped code from mounted volume to function folder
if [ -f /mnt/code/code.tar ]; then
    tar -xf /mnt/code/code.tar -C /usr/local/server/src/function
elif [ -f /mnt/code/code.tar.gz ] || [ -f /mnt/code/code.gz ]; then
    tar -zxf /mnt/code/code.tar.gz -C /usr/local/server/src/function
else
    echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Code archive not found. \e[0m"
    exit 1
fi

# Enter server folder
cd /usr/local/server

. /usr/local/server/helpers/prepare-start.sh

echo 'Starting ...'
