#!/bin/sh
# Fail build if any command fails
set -e

# Merge dependencies
# Apply a (cs|fs|vb)proj file to the root project if found
cp -a /usr/local/build/. /usr/local/server/
cd /usr/local/server
for filename in *.*proj; do
    if [ ! -f "${filename}" ] || [ "${filename}" = "DotNetRuntime.csproj" ]; then
        continue
    fi

    # This is somewhat brittle - won't handle PackageReferences split across multiple lines. 
    # Only occurs in the case of manually editing the csproj file. A solution is parsing the csproj file as XML.
    PACKAGE_REFS=""
    while read line; do
        case "${line}" in *PackageReference*)
            PACKAGE_REFS="\\t\\t${PACKAGE_REFS}${line}\\n"
        esac
    done < "$filename"

    PACKAGE_REFS="\\t<ItemGroup>\\n${PACKAGE_REFS}\\t</ItemGroup>"
    sed -i "s|</Project>|${PACKAGE_REFS}\\n</Project>|" DotNetRuntime.csproj
    rm "$filename"
    break
done

echo "Compiling"

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server
dotnet publish DotNetRuntime.csproj -c Release

# Copy output files
cp -R /usr/local/server/bin/Release/netcoreapp3.1/publish/* /usr/local/build/compiled

echo "Packing build ..."

# Store entrypoint into build. Will be used during start process
touch /usr/local/build/compiled/.open-runtimes
echo "OPEN_RUNTIMES_ENTRYPOINT=$OPEN_RUNTIMES_ENTRYPOINT" > /usr/local/build/compiled/.open-runtimes

# Finish build by preparing tar to use for starting the runtime
tar -C /usr/local/build/compiled --exclude code.tar.gz -zcf /mnt/code/code.tar.gz .

echo "Build finished."