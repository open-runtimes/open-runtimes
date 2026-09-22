#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

echo "Compiling Rust function..."

# Copy user code to function directory
cp -r /usr/local/build/* /usr/local/server/src/function/

# Honor OPEN_RUNTIMES_ENTRYPOINT by rewriting the [lib] path in the user's
# Cargo.toml. Rust resolves the entry file at compile time via Cargo, so this
# substitution is the only place the env var can take effect.
USER_CARGO=/usr/local/server/src/function/Cargo.toml
if [ -n "$OPEN_RUNTIMES_ENTRYPOINT" ] && [ -f "$USER_CARGO" ]; then
	awk -v ep="$OPEN_RUNTIMES_ENTRYPOINT" '
	BEGIN { in_lib = 0; lib_seen = 0 }
	/^\[lib\][[:space:]]*$/ {
		in_lib = 1; lib_seen = 1
		print
		print "path = \"" ep "\""
		next
	}
	in_lib && /^\[/ {
		in_lib = 0
		print
		next
	}
	in_lib && /^[[:space:]]*path[[:space:]]*=/ {
		next
	}
	{ print }
	END {
		if (!lib_seen) {
			print ""
			print "[lib]"
			print "name = \"handler\""
			print "path = \"" ep "\""
		}
	}
	' "$USER_CARGO" >"$USER_CARGO.new"
	mv "$USER_CARGO.new" "$USER_CARGO"
fi

# Compile the code
cd /usr/local/server
cargo build --release

# Move binary to build directory
mv /usr/local/server/target/release/rust_runtime /usr/local/build/rust_runtime
