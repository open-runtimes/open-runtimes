mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

# Runtime-specific files (most)
cp -R ./runtimes/$RUNTIME/* ./runtimes/.test

# Version-specific files
cp -R ./runtimes/$RUNTIME/versions/$VERSION/* ./runtimes/.test