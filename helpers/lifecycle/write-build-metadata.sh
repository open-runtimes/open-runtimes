#!/bin/bash
# Write the `.open-runtimes` metadata file used by start/extract.
#
# Some runtime hooks (e.g. Flutter dart-defines) create `.open-runtimes/` as a
# directory under the build root. When packaging CWD is that same root — empty
# or unresolved OPEN_RUNTIMES_OUTPUT_DIRECTORY — `>` would fail with
# "Is a directory". Clear the Flutter hook directory before writing the
# metadata file; refuse unknown application-owned contents.
#
# Expects: CWD is the packaging directory; OPEN_RUNTIMES_ENTRYPOINT set;
# opr_error available (from lib.sh).

if [ -d .open-runtimes ]; then
	# Flutter build-prepare.sh only writes dart_defines.json into this dir.
	unexpected="$(find .open-runtimes -mindepth 1 -maxdepth 1 ! -name 'dart_defines.json' -print -quit)"
	if [ -n "$unexpected" ]; then
		opr_error "Error: .open-runtimes is a directory with unexpected contents; rename or move it before packaging so build metadata can be written."
		exit 1
	fi
	rm -rf .open-runtimes
fi

# Re-check after clearing the Flutter hook directory. The earlier empty-output
# check can pass when `.open-runtimes/` was the only entry under the build root.
if [ -z "$(ls -A . 2>/dev/null)" ]; then
	opr_error "Error: No build output found. Ensure your output directory isn't empty."
	exit 1
fi

touch .open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" >.open-runtimes
echo "OPEN_RUNTIMES_CLEANUP=${OPEN_RUNTIMES_CLEANUP:-none}" >>.open-runtimes
