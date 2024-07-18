set -e

mkdir -p ./runtimes/.test

# Global files (for all runtimes)
mkdir -p ./runtimes/.test/helpers
cp -R ./helpers/* ./runtimes/.test/helpers

# Runtime base dockerfile
cp -R ./runtimes/$RUNTIME/$RUNTIME.dockerfile ./runtimes/.test

# Runtime-specific files (most)
cp -R ./runtimes/$RUNTIME/versions/latest/* ./runtimes/.test

# Version-specific files
cp -R ./runtimes/$RUNTIME/versions/$VERSION/* ./runtimes/.test

# Global files
cp ./base-before.dockerfile ./runtimes/.test/base-before.dockerfile
cp ./base-after.dockerfile ./runtimes/.test/base-after.dockerfile
cp ./supervisord.conf ./runtimes/.test/supervisord.conf
