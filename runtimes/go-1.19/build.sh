#/bin/sh 

# Fail bild if any command fails
set -e 

# Preapre separate directory to prevent changing user's files
cp -R /usr/code/* /usr/builds
# Get server.go(temporarily) file to build go.mod file
cp /usr/local/src/server/server.go /usr/builds 

# Install User Function dependencies by creting a mod file which holds both user 
# and our server dependencies
cd /usr/builds
go mod init openruntime/server  
go mod tidy 
go mod vendor
go clean -modcache

# compile the code 
go build -o server

# Ready the tar file for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .