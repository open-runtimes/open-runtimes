#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Append User Function Dependencies if cpanfile exists
cd /usr/builds
if [ -f "cpanfile" ]; then
    EXISTING_CPANFILE=$(cat /usr/local/src/cpanfile)
    echo "eval_cpanfile '/usr/builds/cpanfile'" >> /usr/local/src/cpanfile
fi

# Prepare dependencies
cd /usr/local/src/
curl -L https://cpanmin.us | perl - App::cpanminus
export PERL5LIB=./lib:./local/lib/perl5:$PERL5LIB
cpanm -l local --installdeps .
carton install --deployment

if [ -f "cpanfile" ]; then
    echo "${EXISTING_CPANFILE}" > cpanfile
fi

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
cp -R /usr/local/src/local .
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .
