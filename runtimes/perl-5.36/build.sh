#!/bin/sh

# Fail build if any command fails
set -e

# Prepare separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds

# Append User Function Dependencies if cpanfile exists
cd /usr/builds
if [ -f "cpanfile" ]; then
   cat /usr/builds/cpanfile >> /usr/local/src/cpanfile
fi

# Finish build by preparing tar to use for starting the runtime
cd /usr/builds
cp -R /usr/local/src/local .

export PERL5LIB=./lib:./local/lib/perl5:$PERL5LIB
carton install

tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .
