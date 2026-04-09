#!/bin/bash
set -e
shopt -s dotglob

echo "Compiling ..."

mkdir /usr/local/build/compiled

sh gradlew buildJar

cp -R /usr/local/server/build/libs/* /usr/local/build/compiled

if command -v native-image >/dev/null 2>&1; then
	echo "Building native image ..."
	ENTRYPOINT="${OPEN_RUNTIMES_ENTRYPOINT}"
	CLASS_NAME="${ENTRYPOINT%.*}"
	CLASS_NAME="${CLASS_NAME//\//.}"

	cat >/usr/local/build/compiled/reflect-config.json <<REFLECTEOF
[
	{"name": "io.openruntimes.java.${CLASS_NAME}", "allDeclaredMethods": true, "allDeclaredConstructors": true},
	{"name": "io.openruntimes.java.Server", "allDeclaredMethods": true, "allDeclaredConstructors": true},
	{"name": "com.google.gson.internal.LinkedTreeMap", "allDeclaredFields": true, "allDeclaredMethods": true, "allDeclaredConstructors": true}
]
REFLECTEOF

	native-image \
		--no-fallback \
		--enable-url-protocols=http \
		-H:ReflectionConfigurationFiles=/usr/local/build/compiled/reflect-config.json \
		-jar /usr/local/build/compiled/java-runtime-1.0.0.jar \
		-o /usr/local/build/compiled/server

	rm -f /usr/local/build/compiled/java-runtime-1.0.0.jar
	rm -f /usr/local/build/compiled/reflect-config.json
fi
