#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

# Keep only top-level repositories/dependencies/configurations blocks so a
# user build file cannot redefine plugins, tasks or source sets of the server
sanitize_gradle() {
	awk -v sq="'" '
	# Only structural braces may open or close a block, so comments and string
	# literals are removed before counting. Counting them made a commented-out
	# or quoted brace truncate the block and drop real dependencies.
	function code(line,   out, i, n, c, two) {
		out = ""
		n = length(line)
		i = 1
		while (i <= n) {
			c = substr(line, i, 1)
			two = substr(line, i, 2)
			if (block) {
				if (two == "*/") { block = 0; i += 2 } else { i++ }
				continue
			}
			if (quote != "") {
				if (substr(line, i, length(quote)) == quote) { i += length(quote); quote = "" }
				else if (c == "\\" && length(quote) == 1) { i += 2 }
				else { i++ }
				continue
			}
			if (two == "/*") { block = 1; i += 2; continue }
			if (two == "//") { break }
			if (substr(line, i, 3) == "\"\"\"" || substr(line, i, 3) == sq sq sq) { quote = substr(line, i, 3); i += 3; continue }
			if (c == "\"" || c == sq) { quote = c; i++; continue }
			out = out c
			i++
		}
		return out
	}
	BEGIN { capture = 0; depth = 0; block = 0; quote = "" }
	{
		stripped = code($0)
		if (!capture && depth == 0 && stripped ~ /^[[:space:]]*(repositories|dependencies|configurations)[[:space:]]*\{/) {
			capture = 1
		}
		n = length(stripped)
		for (i = 1; i <= n; i++) {
			c = substr(stripped, i, 1)
			if (c == "{") depth++
			if (c == "}") depth--
		}
		if (capture) {
			print
			if (depth <= 0) {
				capture = 0
				depth = 0
			}
		}
	}
	' "$1" >"$1.tmp" && mv "$1.tmp" "$1"
}

# Copy user code to server code
mkdir -p /usr/local/server/src/main/java/io/openruntimes/java
cp -a /usr/local/build/. /usr/local/server/src/main/java/io/openruntimes/java

cd /usr/local/server/src/main/java/io/openruntimes/java

# Maven: resolve dependencies from pom.xml into libs/, picked up by build.gradle
if [ -f "pom.xml" ]; then
	echo "Detected pom.xml. Resolving Maven dependencies..."
	mkdir -p /usr/local/server/libs
	mvn dependency:copy-dependencies -f pom.xml -DoutputDirectory=/usr/local/server/libs --batch-mode --quiet
	rm -f pom.xml
fi

# Gradle: apply the user's build file under a different name so it cannot
# replace the server's own build.gradle
for filename in ./build.gradle ./build.gradle.kts; do
	if [ ! -f "${filename}" ]; then
		continue
	fi
	base=$(basename "${filename}")
	mv "${filename}" "/usr/local/server/user.${base}"
	sanitize_gradle "/usr/local/server/user.${base}"
	echo "apply from: \"user.${base}\"" >>/usr/local/server/build.gradle
done

# Gradle: apply any other user build scripts (for example dependencies.gradle)
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
