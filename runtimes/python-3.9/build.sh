#!/bin/sh

cd /usr/code
python3 -m venv runtime-env
source runtime-env/bin/activate
if [[ -f "requirements.txt" ]]; then
    pip install --no-cache-dir -r requirements.txt
fi

cd /usr/local/src/
pip install --no-cache-dir -r requirements.txt