#!/bin/bash
set -e
shopt -s dotglob

mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

SERVICE_RUNTIMES=("postgres" "mysql" "mongodb")
if [[ " ${SERVICE_RUNTIMES[@]} " =~ " ${RUNTIME} " ]]; then
	cp "./runtimes/$RUNTIME_FOLDER/versions/$VERSION/Dockerfile" ./runtimes/.test/Dockerfile
	cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION"/* ./runtimes/.test
else
	cp -R "./runtimes/$RUNTIME_FOLDER/$RUNTIME_FOLDER.dockerfile" ./runtimes/.test

	cp -R "./runtimes/$RUNTIME_FOLDER/versions/latest"/* ./runtimes/.test

	cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION_FOLDER"/* ./runtimes/.test
fi

# Global Docker configuration (only for function runtimes)
if [[ ! " ${SERVICE_RUNTIMES[@]} " =~ " ${RUNTIME} " ]]; then
	cp ./base-before.dockerfile ./runtimes/.test/base-before.dockerfile
	cp ./base-after.dockerfile ./runtimes/.test/base-after.dockerfile
fi
