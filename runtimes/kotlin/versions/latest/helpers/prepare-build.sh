#!/bin/bash
set -e
shopt -s dotglob

# Extract only safe top-level Gradle blocks (repositories, dependencies), discard everything else
sanitize_gradle() {
	awk '
	BEGIN { capture = 0; capture_depth = 0; file_depth = 0 }
	!capture {
		for (i = 1; i <= length($0); i++) {
			c = substr($0, i, 1)
			if (c == "{") file_depth++
			if (c == "}") file_depth--
		}
	}
	!capture && file_depth == 1 && /(repositories|dependencies|configurations)[[:space:]]*\{/ && !/^[[:space:]]*\/\// {
		capture = 1
		capture_depth = 1
		print
		next
	}
	capture {
		for (i = 1; i <= length($0); i++) {
			c = substr($0, i, 1)
			if (c == "{") capture_depth++
			if (c == "}") capture_depth--
		}
		print
		if (capture_depth <= 0) {
			capture = 0
			capture_depth = 0
			file_depth = 0
		}
	}
	' "$1" > "$1.tmp" && mv "$1.tmp" "$1"
}

# Copy user code to server code
mkdir -p /usr/local/server/src/main/java/io/openruntimes/java
cp -a /usr/local/build/. /usr/local/server/src/main/java/io/openruntimes/java

cd /usr/local/server/src/main/java/io/openruntimes/java

# Maven: resolve dependencies from pom.xml
if [ -f "pom.xml" ]; then
	echo "Detected pom.xml. Resolving Maven dependencies..."
	mkdir -p /usr/local/server/libs
	mvn dependency:copy-dependencies -f pom.xml -DoutputDirectory=/usr/local/server/libs --batch-mode --quiet
	rm -f pom.xml
fi

# Gradle: apply user build files for additional dependencies
for filename in ./build.gradle ./build.gradle.kts; do
	if [ ! -f "${filename}" ]; then
		continue
	fi
	base=$(basename "${filename}")
	mv "${filename}" "/usr/local/server/user.${base}"
	sanitize_gradle "/usr/local/server/user.${base}"
	echo "apply from: \"user.${base}\"" >>/usr/local/server/build.gradle
done

for filename in ./*.gradle ./*.gradle.kts; do
	if [ ! -f "${filename}" ]; then
		continue
	fi
	base=$(basename "${filename}")
	if [ "${base}" = "settings.gradle" ] || [ "${base}" = "settings.gradle.kts" ]; then
		continue
	fi
	mv "${filename}" "/usr/local/server/${filename}"
	sanitize_gradle "/usr/local/server/${filename}"
	echo "apply from: \"${filename}\"" >>/usr/local/server/build.gradle
done
