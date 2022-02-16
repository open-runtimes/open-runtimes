#!/bin/sh

# Prepare separate directory to prevent changign user's files
cp -R /usr/code/* /usr/builds

# Install User Function Dependencies if package.json exists
cd /usr/builds
python3 -m venv runtime-env
source runtime-env/bin/activate
if [[ -f "requirements.txt" ]]; then
    pip install --no-cache-dir -r requirements.txt
fi

# Merge the requirements from server and user code
cd /usr/local/src/
pip install --no-cache-dir -r requirements.txt

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
tar --exclude code.tar.gz -zcvf /usr/code/code.tar.gz .