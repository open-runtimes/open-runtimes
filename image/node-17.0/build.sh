# Install User Function Dependencies if package.json exists
cd /usr/code
if [ -f package.json ]; then
  npm install
fi

mkdir -p node_modules

# Merge the node_modules from the server into the user's node_modules to be restored later.
cp -R /usr/local/src/node_modules/* /usr/code/node_modules