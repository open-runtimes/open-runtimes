#!/bin/sh

cp -a /usr/code/. /usr/local/src/Sources/App/Custom/

if [ ! -z "$ENTRYPOINT_NAME" ]; then
    mv /usr/local/src/Sources/App/Custom/$ENTRYPOINT_NAME /usr/local/src/Sources/App/Custom/userCode.swift
fi

cd /usr/local/src
swift package resolve
swift build --configuration release
rm -r /usr/code/*
cp  "$(swift build --configuration release --show-bin-path)/Run" /usr/code/runtime