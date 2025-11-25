#!/bin/bash
set -e
shopt -s dotglob

mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

SERVICE_RUNTIMES=("postgres" "mysql" "mongodb")
# Check if RUNTIME is in SERVICE_RUNTIMES array
is_service_runtime=false
for service_runtime in "${SERVICE_RUNTIMES[@]}"; do
	if [[ "$service_runtime" == "$RUNTIME" ]]; then
		is_service_runtime=true
		break
	fi
done

if [[ "$is_service_runtime" == "true" ]]; then
	cp "./runtimes/$RUNTIME_FOLDER/versions/$VERSION/Dockerfile" ./runtimes/.test/Dockerfile
	cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION"/* ./runtimes/.test
else
	cp -R "./runtimes/$RUNTIME_FOLDER/$RUNTIME_FOLDER.dockerfile" ./runtimes/.test

	cp -R "./runtimes/$RUNTIME_FOLDER/versions/latest"/* ./runtimes/.test

	cp -R "./runtimes/$RUNTIME_FOLDER/versions/$VERSION_FOLDER"/* ./runtimes/.test
fi

# Global Docker configuration (only for function runtimes)
if [[ "$is_service_runtime" != "true" ]]; then
	cp ./base-before.dockerfile ./runtimes/.test/base-before.dockerfile
	cp ./base-after.dockerfile ./runtimes/.test/base-after.dockerfile
fi
