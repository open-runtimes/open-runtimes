#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

if [ -n "$OPEN_RUNTIMES_OUTPUT_DIRECTORY" ]; then
    cd $OPEN_RUNTIMES_OUTPUT_DIRECTORY
fi

ENTRYPOINT="./server/webpack-runtime.js"
if [ -e "$ENTRYPOINT" ]; then
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m SSR build optimization started. [0m"
    
    mkdir -p /tmp/nft
    cd /tmp/nft
    cp -R /usr/local/server/* .
    mkdir -p ./.next
    cp -R /usr/local/build/.next/* ./.next
    cp -R /usr/local/build/node_modules/* ./node_modules
    
    cp ./src/server-next-js.mjs ./server.mjs
    mkdir -p ./ssr
    cp -R ./src/ssr/* ./ssr/
    
    cp ./ci/nft.mjs .
    
    mkdir -p ./node_modules_min

    node ./nft.mjs
    mkdir -p ./node_modules_min/next
    cp -R ./node_modules/next/* ./node_modules_min/next/
    
    SIZE_BASE=$(du -sb /tmp/nft/node_modules/ | awk '{print $1}')
    SIZE_MIN=$(du -sb /tmp/nft/node_modules_min/ | awk '{print $1}')
    
    SIZE_DIFF=$((SIZE_BASE - SIZE_MIN))
    SIZE_DIFF_MB=$((SIZE_DIFF / 1024 / 1024))
    
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m $SIZE_DIFF_MB MB of node_modules purged. [0m"
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m SSR build optimization finished. [0m"
    
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR started. [0m"
    
    cd /usr/local/build
  
    mkdir -p /tmp/.opr-tmp
    mv ./.next /tmp/.opr-tmp

    mkdir -p ./.next
    cd ./.next
    mv /tmp/.opr-tmp/* .next/

    if [ -d "/usr/local/build/public/" ]; then
        mv /usr/local/build/public/ ./public/
    fi
    
    mv /usr/local/build/package*.json ./
    mv /usr/local/build/next.config.* ./
    mv /tmp/nft/node_modules_min/ ./node_modules/
    
    echo "[90m$(date +[%H:%M:%S]) [31m[[00mopen-runtimes[31m][97m Bundling for SSR finished. [0m"
fi