#!/bin/sh

set -e

build() {
    cp -a /usr/code/. /usr/local/src/

    cd /usr/local/src/

    # Apply a (cs|fs|vb)proj file to the root project if found
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

    # Build the executable
    cd /usr/local/src
    dotnet publish DotNetRuntime.csproj -c Release

    # Tar the executable
    cd /usr/local/src/bin/Release/net6.0/publish/
    tar -czf /usr/code/code.tar.gz .
}

build